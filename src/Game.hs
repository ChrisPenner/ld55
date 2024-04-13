module Game where

import Drawing
import Control.Lens
import Data.Generics.Labels ()
import Data.Word
import FRP.Yampa hiding ((*^))
import GHC.Generics
import SDL hiding (Stereo, Vector, copy)
import Types

data PlayerState = PlayerState
  { ps_position :: V2 Double,
    ps_color :: V4 Word8
  }
  deriving stock (Eq, Ord, Show, Generic)

defaultPlayerState :: PlayerState
defaultPlayerState =
  PlayerState
    { ps_position = 0,
      ps_color = V4 255 0 0 255
    }

game :: SF FrameInfo Renderable
game = proc FrameInfo {fi_controls} -> do
  playerLogic' <- playerLogic -< fi_controls
  returnA -< (drawBackgroundColor (V4 255 0 255 255) <> drawFilledRect (ps_color playerLogic') (Rectangle (P (ps_position playerLogic')) (V2 100 100)))

deltaTime :: SF () Time
deltaTime = loopPre 0 $ proc (_, old) -> do
  now <- localTime -< ()
  returnA -< (now - old, now)


playerLogic :: SF Controller PlayerState
playerLogic = loopPre defaultPlayerState $ proc (c, ps) -> do
  dt <- deltaTime -< ()
  let ps' = ps & #ps_position +~ c_leftStick c * playerSpeed ^* dt
  returnA -< (ps', ps')
  where
    playerSpeed :: V2 Double
    playerSpeed = 100
