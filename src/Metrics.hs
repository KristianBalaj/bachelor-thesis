module Metrics where
    
euclideanDistance :: [Float] -> [Float] -> Float
euclideanDistance lstA lstB = sqrt $ sum (zipWith (\a b -> (a - b) ^ 2) lstA lstB)
