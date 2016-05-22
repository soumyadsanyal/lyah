module Geometry (Point, Shape, area)  where

data Point = Point {x:: Float, y::Float} deriving (Show)
data Shape = Circle {center :: Point, radius:: Float} | Rectangle {bottomleft :: Point, upperright :: Point}  deriving (Show)

area :: Shape -> Float
area (Circle (Point x y) r) = pi*r*r
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs x2-x1) *(abs y2-y1)






