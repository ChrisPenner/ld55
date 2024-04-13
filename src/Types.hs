{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types
  ( module Types,
    Y.Time,
    Y.SF,
    Y.Event,
    Generic,
    module Data.Kind,
    module Data.Typeable,
    Map,
    traceShowId,
  )
where

import Data.Bool (bool)
import Data.Generics.Labels ()
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Word
import Debug.Trace (traceShowId)
import FRP.Yampa (SF)
import FRP.Yampa qualified as Y
import Foreign.C
import GHC.Generics
import SDL hiding (Stereo, Vector, copy)

type Color = V4 Word8

type Renderable = Engine -> IO ()

data Engine = Engine
  { e_renderer :: Renderer,
    e_window :: Window,
    e_resources :: Resources
  }

data Controller = Controller
  { c_leftStick :: V2 Double,
    c_zButton :: Bool,
    c_xButton :: Bool,
    c_cButton :: Bool,
    c_vButton :: Bool,
    c_okButton :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

defaultControls :: Controller
defaultControls =
  Controller
    { c_leftStick = 0,
      c_zButton = False,
      c_xButton = False,
      c_cButton = False,
      c_vButton = False,
      c_okButton = False
    }

data FrameInfo = FrameInfo
  { fi_controls :: [Controller],
    fi_engine :: Engine
  }
  deriving stock (Generic)

type SomeMsg :: (Type -> Type) -> Type
data SomeMsg msg where
  SomeMsg :: Typeable t => msg t -> t -> SomeMsg msg

instance Eq (SomeMsg msg) where
  _ == _ = False

instance (forall a. Show (msg a)) => Show (SomeMsg msg) where
  show (SomeMsg msg _) = "(SomeMsg (" <> show msg <> "))"

instance Show Renderable where
  show _ = "<Renderable>"

instance Show (SF i o) where
  show _ = "<SF>"

type ObjectMap :: (Type -> Type) -> Type -> Type -> Type -> Type
data ObjectMap msg k s a = ObjectMap
  { objm_undeliveredMsgs :: Map k [(k, SomeMsg msg)],
    objm_map :: Map k (s, a)
  }
  deriving stock (Functor, Generic, Foldable)

data Message a
  deriving stock (Eq, Ord, Show, Read, Generic)

type ObjSF msg c k s = SF (ObjectInput msg k s) (ObjectOutput msg c k s)

data DamageSrc = DamageSrc
  { ds_originator :: Key
  , ds_damage :: Int
  , ds_pos :: V2 Double
  , ds_ore :: OriginRect Double
  }
  deriving stock (Eq, Ord, Show, Generic)

data GameMsg t where
  DamageSource :: GameMsg DamageSrc
  Teleport :: GameMsg (V2 Double)

deriving stock instance Eq (GameMsg t)

data GameCommand
  deriving (Eq, Ord, Show, Generic)

data Key
  = Player Int
  | Other Int
  deriving (Eq, Ord, Show)

type Dude = ObjSF GameMsg GameCommand Key GState

data GState = GState
  { gs_position :: V2 Double,
    gs_size :: V2 Double,
    gs_color :: V4 Word8
  }
  deriving stock (Generic)

data Command msg c k s
  = Unspawn
  | Spawn (Maybe k) s (ObjSF msg c k s)
  | Broadcast (SomeMsg msg)
  | OtherCommand c
  deriving stock (Generic)

deriving instance (forall a. Show (msg a), Show k, Show s, Show c) => Show (Command msg c k s)

type ObjectInEvents :: (Type -> Type) -> Type -> Type
data ObjectInEvents msg k = ObjectInEvents
  { oie_mailbox :: forall v. Typeable v => msg v -> [(k, v)]
  }

instance Show (ObjectInEvents msg k) where
  show _ = "<InEvents>"

instance Semigroup (ObjectInEvents msg k) where
  ObjectInEvents a1 <> ObjectInEvents b1 =
    ObjectInEvents (a1 <> b1)

instance Monoid (ObjectInEvents msg k) where
  mempty = ObjectInEvents mempty

type ObjectInput :: (Type -> Type) -> Type -> Type -> Type
data ObjectInput msg k s = ObjectInput
  { oi_fi :: FrameInfo,
    oi_self :: k,
    oi_everyone :: Map k s,
    oi_inbox :: ObjectInEvents msg k
  }
  deriving stock (Generic)

oi_state :: Ord k => ObjectInput msg k s -> s
oi_state oi = fromMaybe (error "uh oh; no state!") $ M.lookup (oi_self oi) $ oi_everyone oi

type ObjectOutput :: (Type -> Type) -> Type -> Type -> Type -> Type
data ObjectOutput msg c k s = ObjectOutput
  { oo_outbox :: [(k, SomeMsg msg)],
    oo_commands :: [Command msg c k s],
    oo_render :: Renderable,
    oo_state :: s
  }
  deriving stock (Generic)

deriving instance (forall a. Show (msg a), Show k, Show s, Show c) => Show (ObjectOutput msg c k s)

instance Semigroup s => Semigroup (ObjectOutput msg c k s) where
  ObjectOutput a1 a2 a3 a4 <> ObjectOutput b1 b2 b3 b4 =
    ObjectOutput
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)
      (a4 <> b4)

deriving via (Ap Y.Event a) instance Semigroup a => Semigroup (Y.Event a)

deriving via (Ap Y.Event a) instance Monoid a => Monoid (Y.Event a)

deriving stock instance Foldable Y.Event

deriving stock instance Traversable Y.Event

data Payload
  = DamageDesc
      { p_damageToDo :: Int,
        p_fireModifier :: Int
      }
  | MovementDesc
      { p_speedModifier :: Float
      }
  deriving stock (Eq, Ord, Show, Generic)

data Spell
  = Projectile Payload (Maybe Spell)
  | Explosion Payload (Maybe Spell)
  | Standard Payload
  | Concurrent Spell Spell
  deriving stock (Eq, Ord, Show, Generic)

type Rune = Either RuneModifier RuneAction

data RuneModifier
  = Rune2x
  | RuneAndThen
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data RuneAction = RuneProjectile | RuneExplosion
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data Finisher = Attack | Defend | Move
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data Resources = Resources
  { r_font :: Char -> Texture,
    r_textures :: GameTexture -> WrappedTexture,
    r_spritesheet :: LpcGuy -> WrappedTexture
  }
  deriving stock (Generic)

data LpcGuy = Wizard
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data WrappedTexture = WrappedTexture
  { getTexture :: Texture,
    wt_sourceRect :: Maybe (Rectangle CInt),
    wt_size :: V2 CInt,
    wt_origin :: V2 CInt
  }
  deriving stock (Generic)

data GameTexture
  = Texture_Rune1
  | Texture_Rune2
  | Texture_Rune3
  | Texture_Rune4
  | Texture_Rune5
  | Texture_Rune6
  | Texture_UnoReverse
  | Texture_UnoSkip
  | Texture_UnoPlusTwo
  | Texture_UnoWild
  | Texture_Background
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data Dir = DirUp | DirLeft | DirDown | DirRight
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data AnimName = SpellCast | Thrust | Walk | Slash | Shoot | Die | Stand
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data Anim = LpcAnim Dir AnimName
  deriving stock (Eq, Ord, Show, Generic)

data DrawSpriteDetails = DrawSpriteDetails
  { dsd_anim :: Anim,
    dsd_rotation :: Double,
    dsd_flips :: V2 Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

data OriginRect aff = OriginRect
  { orect_size :: V2 aff,
    orect_offset :: V2 aff
  }
  deriving (Eq, Ord, Show, Functor, Generic)

orTopLeft :: Num a => V2 a -> OriginRect a -> V2 a
orTopLeft pos ore = pos - orect_offset ore

originRectToRect :: Num a => OriginRect a -> V2 a -> Rectangle a
originRectToRect ore pos =
  Rectangle (P $ orTopLeft pos ore) $
    orect_size ore

wrappedToOriginRect :: WrappedTexture -> OriginRect Double
wrappedToOriginRect wt =
  fmap fromIntegral $
    OriginRect
      { orect_size = wt_size wt,
        orect_offset = wt_origin wt
      }

mkCenterdOriginRect :: Fractional a => V2 a -> OriginRect a
mkCenterdOriginRect sz = OriginRect sz (sz / 2)

mkGroundOriginRect :: Fractional a => V2 a -> OriginRect a
mkGroundOriginRect sz@(V2 x y) = OriginRect sz $ V2 (x / 2) y

onChange :: Eq a => SF a (Y.Event a)
onChange = proc a ->
  Y.edgeBy (\old new -> bool Nothing new $ old /= new) Nothing -< Just a


data ControllerType = Keyboard | Gamepad
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
