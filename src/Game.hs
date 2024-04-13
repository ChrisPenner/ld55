module Game where

import qualified Data.Map as M
import Data.Map (Map)
import Router
import Drawing
import Control.Lens
import Data.Generics.Labels ()
import Data.Word
import FRP.Yampa hiding (now, (*^))
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
game
  = fmap (foldMap oo_render)
  $ router (maybe 0 (+ 1) . fmap fst . M.lookupMax) (\_ _ -> mempty)
  $ ObjectMap mempty $ M.singleton 0 ((), ourDude)

deltaTime :: SF () Time
deltaTime = loopPre 0 $ proc (_, old) -> do
  now <- localTime -< ()
  returnA -< (now - old, now)


playerLogic :: SF Controller PlayerState
playerLogic = loopPre defaultPlayerState $ proc (c, ps) -> do
  dt <- deltaTime -< ()
  let ps' = ps & #ps_position +~ c_leftStick c * playerSpeed ^* dt
  returnA -< dup ps'
  where
    playerSpeed :: V2 Double
    playerSpeed = 100

ourDude :: Dude
ourDude = proc oi -> do
  ps <- playerLogic -< fi_controls $ oi_fi oi
  returnA -< ObjectOutput
    { oo_outbox = mempty
    , oo_commands = mempty
    , oo_render   =
        drawFilledRect (ps_color ps)
          $ Rectangle (P (ps_position ps))
          $ V2 100 100
    , oo_state    = mempty
    }
