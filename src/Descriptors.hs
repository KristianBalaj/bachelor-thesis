module Descriptors (
    featuresVector
) where

import Vec2
import qualified Math

featuresVector :: [Vec2] -> [Float]
featuresVector vertices =
    [
        Descriptors.normalizedLength vertices,
        Descriptors.yPosMax vertices,
        Descriptors.yPosMin vertices,
        Descriptors.yPosStd vertices
    ]
    
-- TODO: float instability issues?
normalizedLength :: [Vec2] -> Float
normalizedLength a = fst (last a) / fromIntegral (length a)

yPosStd :: [Vec2] -> Float
yPosStd = Math.std snd

yPosMax :: [Vec2] -> Float
yPosMax lst = foldl (\b (x, y) -> max y b) (snd $ head lst) lst

yPosMin :: [Vec2] -> Float
yPosMin lst = foldl (\b (x, y) -> min y b) (snd $ head lst) lst

