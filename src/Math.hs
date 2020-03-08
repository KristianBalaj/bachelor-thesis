module Math where

std :: (a -> Float) -> [a] -> Float
std selector lst = 
    let
        average = foldl (\b a -> b + selector a) 0 lst / fromIntegral (length lst)
        squaredDeviationsSum = foldl (\b a -> b + (selector a - average) ^ 2) 0 lst
    in
        sqrt $ squaredDeviationsSum / fromIntegral (length lst)