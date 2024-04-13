{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Drawing where

import SDL hiding (Stereo, Vector)
import Types
import Data.Word
import Data.Foldable (for_)
import Foreign.C


drawOriginRect :: Color -> OriginRect Double -> V2 Double -> Renderable
drawOriginRect c ore = drawFilledRect c . originRectToRect ore


drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c e = do
  let renderer = e_renderer e
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

drawFilledRect :: Color -> Rectangle Double -> Renderable
drawFilledRect c (Rectangle (P v) sz) engine = do
  let rect' = Rectangle (P v) $ sz
  let renderer = e_renderer engine
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap round rect'

drawText :: Double -> V3 Word8 -> String -> V2 Double -> Renderable
drawText sz color text (V2 x y) e
  = do
      let renderer = e_renderer e
      for_ (zip text [0..]) $ \(c, i) -> do
        let glyph = r_font (e_resources e) c
        textureColorMod glyph $= color
        copy renderer glyph Nothing
          $ Just
          $ fmap round
          $ Rectangle (P $ V2 (x + (i * sz)) y)
          $ V2 sz sz
      rendererDrawBlendMode renderer $= BlendAlphaBlend

drawTextureOriginRect
    :: WrappedTexture  -- ^ Texture
    -> OriginRect Double
    -> V2 Double
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> Renderable
drawTextureOriginRect wt ore wp theta flips e
  = do
      let renderer = e_renderer e
      copyEx
        renderer
        (getTexture wt)
        (wt_sourceRect wt)
        (Just $ fmap round $ originRectToRect ore wp)
        (CDouble theta)
        (Just $ P $ fmap round $ orect_offset ore)
        flips


drawGameTextureOriginRect
    :: GameTexture  -- ^ Texture
    -> OriginRect Double
    -> V2 Double
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> Renderable
drawGameTextureOriginRect gt ore wp theta flips e =
  drawTextureOriginRect (r_textures (e_resources e) gt) ore wp theta flips e
