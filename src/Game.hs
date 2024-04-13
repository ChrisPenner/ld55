module Game where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Word
import Drawing
import FRP.Yampa hiding (now, (*^))
import GHC.Generics
import Router
import SDL hiding (Stereo, Vector, copy)
import Types

game :: SF FrameInfo Renderable
game =
  fmap (foldMap oo_render) $
    router (maybe 0 (+ 1) . fmap fst . M.lookupMax) (\_ _ -> mempty) $
      ObjectMap mempty $
        M.singleton 0 (GState {gs_position = 0, gs_color = V4 255 0 0 255, gs_size = 100}, ourDude)

deltaTime :: SF () Time
deltaTime = loopPre 0 $ proc (_, old) -> do
  now <- localTime -< ()
  returnA -< (now - old, now)

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
  let newState = oi_state oi & #gs_position +~ dt *^ velocity
  returnA
    -<
      ObjectOutput
        { oo_outbox = mempty,
          oo_commands = mempty,
          oo_render =
            drawFilledRect (gs_color newState) $
              Rectangle (P (gs_position newState)) $
                gs_size newState,
          oo_state = newState
        }

ourDude :: Dude
ourDude = proc oi -> do
  dPos <- playerLogic -< fi_controls $ oi_fi oi
  let newState = oi_state oi & #gs_position +~ dPos
  returnA
    -<
      ObjectOutput
        { oo_outbox = mempty,
          oo_commands = mempty,
          oo_render = renderGState newState,
          oo_state = newState
        }

renderGState :: GState -> Renderable
renderGState gs =
  drawFilledRect (gs_color gs) $
    Rectangle (P (gs_position gs)) $
      gs_size gs
