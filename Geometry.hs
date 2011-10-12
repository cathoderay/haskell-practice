module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where


sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)


sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2) 


rectArea :: Float -> Float -> Float
rectArea a b = a * b


cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c


cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = sum $ map (*2) [rectArea a b, rectArea a c, rectArea c b]


cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side


cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side


