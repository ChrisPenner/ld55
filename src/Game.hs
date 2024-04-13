{-# LANGUAGE BlockArguments #-}

module Game where

import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Word
import Drawing
import FRP.Yampa hiding (normalize, (*^))
import GHC.Generics
import ParseSpell (parseSpell)
import Router
import SDL hiding (Event, Stereo, Vector, copy)
import Types

game :: SF FrameInfo Renderable
game =
  fmap (\z ->
    mconcat
     [ z
     , drawText 16 (V3 0 0 0) "yo what up" 30
     , drawGameTextureOriginRect Texture_Rune1 (mkCenterdOriginRect 50) (V2 100 500) 0 (pure False)
     , drawGameTextureOriginRect Texture_Rune2 (mkCenterdOriginRect 50) (V2 200 500) 0 (pure False)
     , drawGameTextureOriginRect Texture_Rune3 (mkCenterdOriginRect 50) (V2 300 500) 0 (pure False)
     , drawGameTextureOriginRect Texture_Rune4 (mkCenterdOriginRect 50) (V2 400 500) 0 (pure False)
     , drawGameTextureOriginRect Texture_Rune5 (mkCenterdOriginRect 50) (V2 500 500) 0 (pure False)
     , drawGameTextureOriginRect Texture_Rune6 (mkCenterdOriginRect 50) (V2 600 500) 0 (pure False)
     ]
       ) $
  fmap (foldMap oo_render) $
    router (maybe 0 (+ 1) . fmap fst . M.lookupMax) mempty $
      ObjectMap mempty $ M.fromList $
        (0, (GState {gs_position = 200, gs_color = V4 255 0 0 255, gs_size = 30}, ourDude))
        : zip [1..] (fmap (GState {gs_position = 200, gs_color = V4 255 0 0 255, gs_size = 30},)
               $ makeSpells (V2 1 1)
               $ traceShowId $ parseSpell
                        Attack
                        [ Rune2x,
                          RuneProjectile,
                          RuneAndThen,
                          RuneNegate,
                          RuneExplosion
                        ]
                    )

deltaTime :: SF () Time
deltaTime = loopPre 0 $ proc (_, old) -> do
  nowish <- localTime -< ()
  returnA -< (nowish - old, nowish)

playerLogic :: SF (Controller) (V2 Double)
playerLogic = proc c -> do
  dt <- deltaTime -< ()
  returnA -< c_leftStick c * playerSpeed ^* dt
  where
    playerSpeed :: V2 Double
    playerSpeed = 100

data FireballState = FireballState
  { fs_position :: V2 Double,
    fs_velocity :: V2 Double,
    fs_color :: V4 Word8
  }
  deriving stock (Eq, Ord, Show, Generic)

fireBall :: V2 Double -> Dude
fireBall velocity = proc oi -> do
  dt <- deltaTime -< ()
  t <- localTime -< ()
  let timeLeft = ttl - t
  e <- delayEvent ttl <<< now () -< ()
  let newState =
        oi_state oi
          & #gs_position +~ dt *^ velocity
          & #gs_color . _w .~ round (timeLeft * 255 / ttl)
  let commands = e & foldMap \() -> [Unspawn]
  returnA
    -<
      ObjectOutput
        { oo_outbox = mempty,
          oo_commands = commands,
          oo_render = renderGState newState,
          oo_state = newState
        }
  where
    ttl = 2

ourDude :: Dude
ourDude = proc oi -> do
  dPos <- playerLogic -< fi_controls $ oi_fi oi
  okPressed <- edge -< c_okButton . fi_controls $ oi_fi oi
  dirFacing <-
    hold (V2 1 0)
      <<< edgeBy
        ( \_ vel ->
            if vel /= 0
              then Just vel
              else Nothing
        )
        0
      -<
        oi ^. #oi_fi . #fi_controls . #c_leftStick

  let commands =
        okPressed & foldMap \() ->
          [ Spawn Nothing (GState {gs_position = gs_position $ oi_state oi, gs_color = V4 0 255 0 254, gs_size = 20}) $ fireBall (dirFacing * 100)
          ]

  let newState = oi_state oi & #gs_position +~ dPos
  returnA
    -<
      ObjectOutput
        { oo_outbox = mempty,
          oo_commands = commands,
          oo_render = renderGState newState,
          oo_state = newState
        }

renderGState :: GState -> Renderable
renderGState gs =
  drawFilledRect (gs_color gs) $
    Rectangle (P (gs_position gs)) $
      gs_size gs

withLifetime :: Time -> SF a (Double, Event (), [Command msg c k s])
withLifetime ttl = proc _ -> do
  t <- localTime -< ()
  e <- delayEvent ttl <<< now () -< ()
  returnA -< (t / ttl, e, onEvent' e Unspawn)

spellTtl :: Double
spellTtl = 0.5

onEvent :: (Applicative f, Monoid (f b)) => Event a -> (a -> b) -> f b
onEvent ev f = foldMap (pure . f) ev

onEvent' :: (Applicative f, Monoid (f b)) => Event a -> b -> f b
onEvent' ev = onEvent ev . const

spellContinuation ::
  Time ->
  V2 Double ->
  Maybe Spell ->
  SF GState (Double, [Command GameMsg GameCommand Key GState])
spellContinuation ttl dir k = proc s -> do
  (perc, on_die, die_cmds) <- withLifetime ttl -< ()
  returnA
    -<
      ( perc,
        mconcat
          [ die_cmds,
            join $ onEvent' on_die $ do
              spell <- maybeToList k
              dude <- makeSpells dir spell
              pure $ Spawn Nothing s dude
          ]
      )

makeSpells :: V2 Double -> Spell -> [Dude]
makeSpells _ (Standard _) = []
makeSpells (normalize -> dir) (Projectile _ k) =
  pure $ proc oi -> do
    (_, cmds) <- spellContinuation spellTtl dir k -< oi_state oi
    dt <- deltaTime -< ()
    let st =
          oi_state oi
            & #gs_position +~ dir * 400 ^* dt
            & #gs_size .~ 10
            & #gs_color .~ V4 0 0 0 255
    returnA
      -<
        ObjectOutput
          { oo_outbox = mempty,
            oo_commands = cmds,
            oo_render = renderGState st,
            oo_state = st
          }
makeSpells dir (Explosion _ k) =
  pure $ proc oi -> do
    (perc, cmds) <- spellContinuation spellTtl dir k -< oi_state oi
    let st = oi_state oi
          & #gs_size .~ perc *^ 100
          & #gs_color .~ V4 255 0 255 128
    returnA -<
      ObjectOutput
        { oo_outbox = mempty
        , oo_commands = cmds
        , oo_render = renderGState st
        , oo_state = st
        }
makeSpells dir (Concurrent x y) =
  makeSpells (mkRotMatrix (pi / 10) !* dir) x
   <> makeSpells (mkRotMatrix (-pi / 10) !* dir) y


mkRotMatrix :: Double -> M22 Double
mkRotMatrix theta =
  V2
    (V2 (cos theta) (negate $ sin theta))
    (V2 (sin theta) (cos theta))

