module Vec2 where

type Vec2 = (Float, Float)

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (a, b) (c, d) = (a + c, b + d)