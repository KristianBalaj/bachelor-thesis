module Descriptors (
    normalizedLength,
    yPosStd,
    yPosMax,
    yPosMin
) where

import Vec2
import qualified Math

-- TODO: float instability issues?
normalizedLength :: [Vec2] -> Float
normalizedLength a = fst (last a) / fromIntegral (length a)

yPosStd :: [Vec2] -> Float
yPosStd = Math.std snd

yPosMax :: [Vec2] -> Float
yPosMax lst = foldl (\b (x, y) -> max y b) (snd $ head lst) lst

yPosMin :: [Vec2] -> Float
yPosMin lst = foldl (\b (x, y) -> min y b) (snd $ head lst) lst
