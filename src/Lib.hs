{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib where

import Game
import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.IORef
import Data.Time.Clock.System
import FRP.Yampa hiding (normalize, (*^))
import SDL hiding (Stereo, Vector, copy)
import System.Exit
import Types
import Resources (loadResources)
import Data.Traversable
import Data.Foldable
import Data.Int
import SDL.Input.GameController (ControllerButton(..))
import SDL.Internal.Numbered (toNumber)

main :: IO ()
main = do
  -- ALUT.withProgNameAndArgs ALUT.runALUT $ \_ _ -> do
  initializeAll

  jsds <- fmap toList availableJoysticks
  jss <- for jsds openJoystick
  let num_players = 1 + length jss

  window <-
    createWindow "ld55" $
      defaultWindow
        { windowInitialSize = fmap (round @Double) screenSize,
          windowGraphicsContext = OpenGLContext defaultOpenGL
        }
  ctx <- glCreateContext window
  glMakeCurrent window ctx
  renderer <-
    createRenderer
      window
      (-1)
      defaultRenderer
        { rendererType = AcceleratedRenderer,
          rendererTargetTexture = True
        }
  rendererScale renderer $= screenSize / logicalSize
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  cursorVisible $= False

  resources <- loadResources renderer

  let engine =
        Engine
          { e_renderer = renderer,
            e_window = window
          , e_resources = resources
          }

  tS <- getSystemTime
  let seconds = floatSeconds tS
  tRef <- newIORef seconds

  reactimate
    (pure $ FrameInfo (replicate num_players defaultControls) engine)
    (input engine tRef jss)
    (output engine)
    (game num_players)
  quit

parseControls :: (Scancode -> Bool) -> Controller
parseControls isKeyDown =
  Controller
    { c_leftStick =
        V2
          ( if
                | isKeyDown ScancodeA -> -1
                | isKeyDown ScancodeD -> 1
                | otherwise -> 0
          )
          ( if
                | isKeyDown ScancodeW -> -1
                | isKeyDown ScancodeS -> 1
                | otherwise -> 0
          )
          & SDL.normalize
    , c_okButton = isKeyDown ScancodeSpace
    , c_zButton = isKeyDown ScancodeZ
    , c_xButton = isKeyDown ScancodeX
    , c_cButton = isKeyDown ScancodeC
    , c_vButton = isKeyDown ScancodeV
    }


controllerButton :: Joystick -> ControllerButton -> IO Bool
controllerButton js = buttonPressed js . fromIntegral . toNumber

parseController :: Joystick -> IO Controller
parseController js = do
  a <- controllerButton js ControllerButtonA
  b <- controllerButton js ControllerButtonB
  x <- controllerButton js ControllerButtonX
  y <- controllerButton js ControllerButtonY
  l <- controllerButton js ControllerButtonBack
  r <- controllerButton js ControllerButtonGuide
  ok <- controllerButton js ControllerButtonRightShoulder

  dx <- axisPosition js 0
  dy <- axisPosition js 1
  pure $ Controller
    { c_zButton = a
    , c_xButton = b
    , c_cButton = x
    , c_vButton = y
    , c_okButton = ok
    , c_leftStick = normalize $ fmap ((/ 32767)) $ fmap (\z -> if abs z < 8000 then 0 else z) $ fmap fromIntegral $ V2 dx dy
    }


clampAxis :: Int16 -> Int
clampAxis (fromIntegral @_ @Int -> i) =
  if abs i <= 8000
     then 0
     else signum i

input :: Engine -> IORef Double -> [Joystick] -> Bool -> IO (Double, Maybe FrameInfo)
input engine tRef jss _ = do
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
  js <- for jss parseController

  pure (dt, Just $ FrameInfo (parseControls keys : js) engine)

pattern Keypress :: Scancode -> EventPayload
pattern Keypress scan <- KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym scan _ _))

isQuit :: EventPayload -> Bool
isQuit QuitEvent = True
isQuit (WindowClosedEvent _) = True
isQuit (Keypress ScancodeEscape) = True
isQuit (Keypress ScancodeCapsLock) = True
isQuit _ = False

output :: Engine -> Bool -> Renderable -> IO Bool
output e _ render = do
  let renderer = e_renderer e
  rendererDrawColor renderer $= V4 100 149 237 255
  clear renderer
  render e
  present renderer
  pure False

floatSeconds :: SystemTime -> Double
floatSeconds t =
  fromIntegral (systemSeconds t)
    + fromIntegral (systemNanoseconds t) / 1e9

