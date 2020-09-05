module Descriptors (
    featuresVector
) where

import Data.List

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
normalizedLength a = Vec2.vecX (last a) / fromIntegral (length a)

yPosStd :: [Vec2] -> Float
yPosStd = Math.std Vec2.vecY

yPosMax :: [Vec2] -> Float
yPosMax = foldl' (\b (Vec2 _ y) -> max y b) 0

yPosMin :: [Vec2] -> Float
yPosMin = foldl' (\b (Vec2 _ y) -> min y b) 0

