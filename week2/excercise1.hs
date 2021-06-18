{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

drawColoredSquare :: Color -> Picture
drawColoredSquare color = colored color (solidRectangle 1 1)

wall = drawColoredSquare gray

ground = drawColoredSquare yellow

storage = colored black (solidCircle 0.25) & ground

box = drawColoredSquare brown

data Tile
  = Wall
  | Ground
  | Storage
  | Box
  | Blank
  deriving (Eq)

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

axis :: [Double]
axis = [-10 .. 10]

cartProd xs ys = [(x, y) | x <- xs, y <- ys]

matrix = cartProd axis axis

pictureOfMaze :: Picture
pictureOfMaze =
  foldl1 (&) (fmap (\(x, y) -> translated x y (drawTile (maze (C x y)))) matrix)

data Direction
  = R
  | U
  | L
  | D

data Coord =
  C Double Double

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated x y

atAllowedCoord :: Coord -> Bool
atAllowedCoord c
  | currentTile == Ground || currentTile == Storage = True
  | otherwise = False
  where
    currentTile = maze c

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)

conditionalMove :: Coord -> Coord -> Coord
conditionalMove coordAfterMove coordBeforeMove
  | atAllowedCoord coordAfterMove = coordAfterMove
  | otherwise = coordBeforeMove

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
  | key == "Right" = conditionalMove (adjacentCoord R c) c
  | key == "Up" = conditionalMove (adjacentCoord U c) c
  | key == "Left" = conditionalMove (adjacentCoord L c) c
  | key == "Down" = conditionalMove (adjacentCoord D c) c
handleEvent _ c = c

drawState :: Coord -> Picture
drawState c = atCoord c player & pictureOfMaze

-- Player function is not mine
player :: Picture
player =
  translated 0 0.3 cranium & polyline [(0, 0), (0.3, 0.05)] &
  polyline [(0, 0), (0.3, -0.05)] &
  polyline [(0, -0.2), (0, 0.1)] &
  polyline [(0, -0.2), (0.1, -0.5)] &
  polyline [(0, -0.2), (-0.1, -0.5)]
  where
    cranium = circle 0.18 & sector (7 / 6 * pi) (1 / 6 * pi) 0.18

main :: IO ()
main = activityOf initialCoord handleEvent drawState
