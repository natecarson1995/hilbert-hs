module Main where
import Data.Fixed

data Point = Point Float Float

instance Show Point where
    show (Point x y) = show x ++ ", " ++ show y
addPoints :: Point -> Point -> Point
addPoints (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

scalePoint :: Float -> Point -> Point
scalePoint scale (Point x y) = Point (x * scale) (y * scale)

lerp :: Point -> Point -> Float -> Point
lerp (Point srcX srcY) (Point destX destY) progressAlong = 
    Point (srcX + ((destX - srcX) * progressAlong)) (srcY + ((destY - srcY) * progressAlong))

translateFloatToIndexAndMod :: Point -> (Int, Float)
translateFloatToIndexAndMod (Point input stepSize) = (floor (input / stepSize), (mod' input stepSize) * (1.0/stepSize))

lerpPointAndNext :: [Point] -> (Int, Float) -> Point
lerpPointAndNext points (index, progressAlong) = lerp (points!!index) (points!!(index+1)) progressAlong

lerpPointArray :: [Point] -> Float -> Point
lerpPointArray points 1.0 = last points
lerpPointArray points progressAlong =
    lerpPointAndNext points (translateFloatToIndexAndMod (Point progressAlong (1.0 / (fromIntegral ((length points) - 1)))))

unfoldr :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []

phcPoints :: Int -> [Point]
phcPoints 1 = [Point 0.0 0.0, Point 0.0 1.0, Point 1.0 1.0, Point 1.0 0.0]
phcPoints x = map (scalePoint 0.33333) $ concat [
    rotateLeftNormals (phcPoints (x-1)), 
    map (addPoints (Point 0.0 2.0)) (phcPoints (x-1)),
    map (addPoints (Point 2.0 2.0)) (phcPoints (x-1)),
    map (addPoints (Point 2.0 0.0)) (rotateRightNormals (phcPoints (x-1)))]

phc :: Int -> Float -> Point
phc order x = lerpPointArray (phcPoints order) x

hc :: Float -> [Point]
hc x = unfoldr (\order -> Just (lerpPointArray (phcPoints order) x, order+1)) 1

rotateRight :: Point -> Point
rotateRight (Point x y) = Point (-y) x

rotateLeft :: Point -> Point
rotateLeft (Point x y) = Point y (-x)

unNorm :: Point -> Point
unNorm = addPoints (Point (-0.5) (-0.5))

norm :: Point -> Point
norm = addPoints (Point 0.5 0.5)

rotateLeftNormal :: Point -> Point
rotateLeftNormal point = norm $ rotateLeft $ unNorm point

rotateLeftNormals :: [Point] -> [Point]
rotateLeftNormals points = reverse $ map rotateLeftNormal points

rotateRightNormal :: Point -> Point
rotateRightNormal point = norm $ rotateRight $ unNorm point

rotateRightNormals :: [Point] -> [Point]
rotateRightNormals points = reverse $ map rotateRightNormal points


swapPoint :: Point -> Point
swapPoint (Point x y) = Point y x

main :: IO()
main = putStrLn $ unlines $ map show $ map (phc 3) [0,0.001..1.0]