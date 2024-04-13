{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib where

import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.IORef
import Data.Time.Clock.System
import Data.Word
import FRP.Yampa hiding ((*^))
import GHC.Generics
import SDL hiding (Stereo, Vector, copy)
import System.Exit
import Types

aspectRatio :: RealFloat a => a
aspectRatio = 16 / 9

logicalSize :: RealFloat a => V2 a
logicalSize = screenSize

screenSize :: RealFloat a => V2 a
screenSize = V2 (h * aspectRatio) h
  where
    h = 540

main :: IO ()
main = do
  -- ALUT.withProgNameAndArgs ALUT.runALUT $ \_ _ -> do
  initializeAll

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

  let engine =
        Engine
          { e_renderer = renderer,
            e_window = window
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

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c e = do
  let renderer = e_renderer e
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

game :: SF FrameInfo Renderable
game = proc FrameInfo {fi_controls} -> do
  playerLogic' <- playerLogic -< fi_controls
  returnA -< (drawBackgroundColor (V4 255 0 255 255) <> drawFilledRect (ps_color playerLogic') (Rectangle (P (ps_position playerLogic')) (V2 100 100)))

drawFilledRect :: Color -> Rectangle Double -> Renderable
drawFilledRect c (Rectangle (P v) sz) engine = do
  let rect' = Rectangle (P v) $ sz
  let renderer = e_renderer engine
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap round rect'

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

playerLogic :: SF Controller PlayerState
playerLogic = loopPre defaultPlayerState $ proc (c, ps) -> do
  t <- localTime -< ()
  dt <- derivative -< t
  let ps' = ps & #ps_position +~ c_leftStick c * playerSpeed ^* dt
  returnA -< (ps', ps')
  where
    playerSpeed :: V2 Double
    playerSpeed = 10

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
          & SDL.normalize,
      c_okButton = isKeyDown ScancodeZ,
      c_cancelButton = isKeyDown ScancodeX
    }

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

