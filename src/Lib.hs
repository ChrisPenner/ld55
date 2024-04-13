{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Lib where

import Data.Word
import FRP.Yampa
import           Control.Monad
import           Data.Foldable (fold)
import           Data.IORef
import           Data.Time.Clock.System
import           Data.Traversable (for)
import           GHC.Generics
import           SDL hiding (Vector, copy, Stereo)
import qualified Sound.ALUT as ALUT
import           System.Exit

aspectRatio :: RealFloat a => a
aspectRatio = 16 / 9

logicalSize :: RealFloat a => V2 a
logicalSize = screenSize

screenSize :: RealFloat a => V2 a
screenSize = V2 (h * aspectRatio) h
  where
    h = 540

data Engine = Engine
  { e_renderer :: Renderer
  , e_window :: Window
  }

main :: IO ()
main = do -- ALUT.withProgNameAndArgs ALUT.runALUT $ \_ _ -> do
  initializeAll

  window <- createWindow "ld55" $ defaultWindow
    { windowInitialSize = fmap (round @Double) screenSize
    , windowGraphicsContext = OpenGLContext defaultOpenGL
    }
  ctx <- glCreateContext window
  glMakeCurrent window ctx
  renderer <- createRenderer window (-1) defaultRenderer
    { rendererType = AcceleratedRenderer
    , rendererTargetTexture = True
    }
  rendererScale renderer $= screenSize / logicalSize
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  cursorVisible $= False

  let engine = Engine
        { e_renderer = renderer
        , e_window = window
        }

  tS <- getSystemTime
  let seconds = floatSeconds tS
  tRef <- newIORef seconds

  reactimate
    (pure $ FrameInfo defaultControls engine)
    (input engine tRef Nothing)
    (output engine)
    game
  quit

type Color = V4 Word8

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c e = do
  let renderer = e_renderer e
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

game :: SF FrameInfo Renderable
game = pure $ drawBackgroundColor $ V4 255 0 255 255

data Controller = Controller
  { c_leftStick :: V2 Float
  , c_okButton :: Bool
  , c_cancelButton :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

defaultControls :: Controller
defaultControls = Controller
  { c_leftStick = 0
  , c_okButton = False
  , c_cancelButton = False
  }

data FrameInfo = FrameInfo
  { fi_controls :: Controller
  , fi_engine :: Engine
  }
  deriving stock (Generic)

parseControls :: (Scancode -> Bool) -> Controller
parseControls _ = defaultControls

input :: Engine -> IORef Double -> Maybe Joystick -> Bool -> IO (Double, Maybe FrameInfo)
input engine tRef _ _ = do
  let win = e_window engine
  pumpEvents
  es <- pollEvents
  when (any (isQuit . eventPayload) es) $ do
    destroyWindow win
    exitSuccess
  seconds <- readIORef tRef
  tS <- getSystemTime
  let seconds' = floatSeconds tS
  writeIORef tRef seconds'

  let dt = seconds' - seconds

  keys <- getKeyboardState
  -- js <- for mjs parseController

  pure (dt, Just $ FrameInfo (parseControls keys) engine)


pattern Keypress :: Scancode -> EventPayload
pattern Keypress scan <- KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym scan _ _))


isQuit :: EventPayload -> Bool
isQuit QuitEvent                   = True
isQuit (WindowClosedEvent _)       = True
isQuit (Keypress ScancodeEscape)   = True
isQuit (Keypress ScancodeCapsLock) = True
isQuit _                           = False


output :: Engine -> Bool -> Renderable -> IO Bool
output e _ render = do
  let renderer = e_renderer e
  rendererDrawColor renderer $= V4 100 149 237 255
  clear renderer
  render e
  present renderer
  pure False


floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9


type Renderable = Engine -> IO ()

