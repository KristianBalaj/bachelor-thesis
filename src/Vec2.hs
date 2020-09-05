module Vec2 where

data Vec2 = Vec2 { vecX :: !Float, vecY :: !Float }

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)

-- type Vec2 = (Float, Float)

-- addVec2 :: Vec2 -> Vec2 -> Vec2
-- addVec2 (a, b) (c, d) = (a + c, b + d)