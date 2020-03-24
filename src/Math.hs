module Math where

std :: (a -> Float) -> [a] -> Float
std selector lst = 
    let
        lstSum = foldl (\b a -> b + selector a) 0 lst
        average = lstSum / fromIntegral (length lst)
        squaredDeviationsSum = foldl (\b a -> b + (selector a - average) ^ 2) 0 lst
    in
        sqrt $ squaredDeviationsSum / fromIntegral (length lst)