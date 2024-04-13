{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Drawing where

import SDL hiding (Stereo, Vector, copy)
import Types

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

