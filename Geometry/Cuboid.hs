module Geometry.Cuboid
( volume
, area
) where


volume :: Float -> Float -> Float -> Float
voluem a b c = rectArea a b * c


area :: Float -> Float -> Float -> Float
area a b c = sum $ map (*2) [rectArea a b, rectArea a c, rectArea c b]


rectArea :: Float -> Float -> Float
rectArea a b = a * b
