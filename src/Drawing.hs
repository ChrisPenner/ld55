{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Drawing where

import SDL hiding (Stereo, Vector)
import Types
import Data.Word
import Data.Foldable (for_)
import Foreign.C
import FRP.Yampa

getLpcAnim :: Dir -> AnimName -> Int -> WrappedTexture -> WrappedTexture
getLpcAnim dir anim frame wt = wt
  { wt_sourceRect
      = Just $ Rectangle
          (P $ V2 (fromIntegral frame)
                  (fromIntegral $ fromEnum anim * 4 + fromEnum dir)
                * 64)
          64
  , wt_size = 64
  , wt_origin = V2 32 64
  }

drawLPC :: LpcGuy -> Dir -> AnimName -> V2 Double -> Int -> Double -> Renderable
drawLPC guy dir anim pos fr rot e =
  drawSpriteStretched
    (getLpcAnim dir anim fr $ flip r_spritesheet guy $ e_resources e)
    pos
    rot (pure False) 1 e

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

drawSpriteStretched
    :: WrappedTexture  -- ^ Texture
    -> V2 Double       -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> V2 Double       -- ^ scaling factor
    -> Renderable
drawSpriteStretched wt pos theta flips stretched e = do
  let wp = pos - (fmap fromIntegral (wt_origin wt) * stretched)
  let renderer = e_renderer e
  copyEx
    renderer
    (getTexture wt)
    (wt_sourceRect wt)
    (Just $ fmap round
          $ Rectangle (P wp)
          $ fmap fromIntegral (wt_size wt) * stretched)
    (CDouble theta)
    (Just $ fmap round
          $ P
          $ fmap fromIntegral (wt_origin wt) * stretched)
    flips

frameCounts :: AnimName -> Int
frameCounts SpellCast = 7
frameCounts Thrust = 8
frameCounts Walk = 9
frameCounts Slash = 6
frameCounts Shoot = 13
frameCounts Die = 6

mkAnim :: LpcGuy -> SF (DrawSpriteDetails, V2 Double) Renderable
mkAnim guy = proc (dsd, pos) -> do
  let LpcAnim dir anim = dsd_anim dsd
  global_tick <- round . (/ 0.1) <$> localTime -< ()
  new_anim <- onChange -< dsd_anim dsd
  anim_start <- hold 0 -< global_tick <$ new_anim

  let anim_frame = (global_tick - anim_start) `mod` frameCounts anim
  new_frame <- onChange -< anim_frame

  returnA -< drawLPC guy dir anim pos anim_frame 0
