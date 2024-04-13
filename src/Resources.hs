module Resources where

import qualified Data.Map as M
import System.FilePath
import System.Environment.Blank
import Types
import Data.Traversable
import SDL.JuicyPixels (loadJuicyTexture)
import SDL.Video (Renderer, TextureInfo (..))
import SDL (Texture, queryTexture)
import Linear

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

  fontmap <- fmap M.fromList $ for [32 .. 122] $ \i -> do
    t <- loadJuicyTexture renderer $ rpath </> "font" </> "font-" <> pad 3 '0' (show i) <.> "png"
    pure (toEnum i, t)

  texmap <- fmap M.fromList $ for [minBound .. maxBound] $ \gt -> do
    t <- loadJuicyTexture renderer $ rpath </> "sprites" </> textureName gt <.> "png"
    wt <- wrapTexture t
    pure (gt, wt)

  pure $ Resources
    { r_font = (fontmap M.!)
    , r_textures = (texmap M.!)
    }

textureName :: GameTexture -> String
textureName Texture_Rune1 = "rune1"
textureName Texture_Rune2 = "rune2"
textureName Texture_Rune3 = "rune3"
textureName Texture_Rune4 = "rune4"
textureName Texture_Rune5 = "rune5"
textureName Texture_Rune6 = "rune6"
textureName Texture_UnoSkip = "uno-skip"
textureName Texture_UnoPlusTwo = "uno-plus2"
textureName Texture_UnoReverse = "uno-reverse"
textureName Texture_UnoWild = "uno-wild"

wrapTexture :: Texture -> IO WrappedTexture
wrapTexture t = do
  q <- queryTexture t
  pure $ WrappedTexture
    { getTexture = t
    , wt_size = V2 (textureWidth q) $ textureHeight q
    , wt_sourceRect = Nothing
    , wt_origin = 0
    }
