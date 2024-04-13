module Resources where

import qualified Data.Map as M
import System.FilePath
import System.Environment.Blank
import Types
import Data.Traversable
import SDL.JuicyPixels (loadJuicyTexture)
import SDL.Video (Renderer)

resourceRootPath :: IO FilePath
resourceRootPath =
  maybe "resources" (</> "usr/share/ld55-exe/resources") <$> getEnv "APPDIR"

pad :: Int -> Char -> String -> String
pad n c s =
  let len = length s
   in case len >= n of
        True -> s
        False -> replicate (n - len) c <> s

loadResources :: Renderer -> IO Resources
loadResources renderer = do
  rpath <- resourceRootPath

  font <- fmap M.fromList $ for [32 .. 122] $ \i -> do
    t <- loadJuicyTexture renderer $ rpath </> "font" </> "font-" <> pad 3 '0' (show i) <.> "png"
    pure (toEnum i, t)

  pure $ Resources
    { r_font = (font M.!)
    }
