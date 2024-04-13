{-# LANGUAGE BlockArguments #-}

module Game where

import Control.Monad
import Control.Lens
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Word
import Drawing
import FRP.Yampa hiding (normalize, (*^))
import GHC.Generics
import Router
import SDL hiding (Event, Stereo, Vector, copy)
import Types
import Data.Maybe (maybeToList)

game :: SF FrameInfo Renderable
game =
  fmap (foldMap oo_render) $
    router (maybe 0 (+ 1) . fmap fst . M.lookupMax) mempty $
      ObjectMap mempty $ M.fromList $
        [ (0, (GState {gs_position = 0, gs_color = V4 255 0 0 255, gs_size = 30}, ourDude))
        , (1, (GState {gs_position = 300, gs_color = V4 255 0 0 255, gs_size = 30}, head
               $ makeSpells (V2 1 1)
               $ Projectile undefined
               $ Just
               $ Explosion undefined
               $ Just
               $ Projectile undefined Nothing
          ))
        ]

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


makeSpells :: V2 Double -> Spell -> [Dude]
makeSpells _ (Standard _) = []
makeSpells (normalize -> dir) (Projectile _ k) =
  pure $ proc oi -> do
    (perc, on_die, die_cmds) <- withLifetime spellTtl -< ()
    dt <- deltaTime -< ()
    let st = oi_state oi
          & #gs_position +~ dir * 400 ^* dt
          & #gs_size .~ 10
          & #gs_color .~ V4 0 0 0 255
    returnA -<
      ObjectOutput
        { oo_outbox = mempty
        , oo_commands = mconcat
            [ die_cmds
            , join $ onEvent' on_die $ do
                spell <- maybeToList k
                dude <- makeSpells dir spell
                pure $ Spawn Nothing st dude
            ]
        , oo_render = renderGState st
        , oo_state = st
        }

makeSpells dir (Explosion _ k) =
  pure $ proc oi -> do
    (perc, on_die, die_cmds) <- withLifetime spellTtl -< ()
    let st = oi_state oi
          & #gs_size .~ perc *^ 100
          & #gs_color .~ V4 255 0 255 128
    returnA -<
      ObjectOutput
        { oo_outbox = mempty
        , oo_commands = mconcat
            [ die_cmds
            , join $ onEvent' on_die $ do
                spell <- maybeToList k
                dude <- makeSpells dir spell
                pure $ Spawn Nothing st dude
            ]
        , oo_render = renderGState st
        , oo_state = st
        }
makeSpells dir (Concurrent x y) = makeSpells dir x <> makeSpells dir y


