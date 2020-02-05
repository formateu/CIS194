{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = colored black (solidCircle 0.3) & ground
box =     colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 0 = blank
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

pictureOfMaze :: Picture
pictureOfMaze = drawRows (-10)

drawRows :: Integer -> Picture
drawRows 11 = blank
drawRows r  = drawCols r (-10) & drawRows (r+1)

drawCols :: Integer -> Integer -> Picture
drawCols _ 11 = blank
drawCols r c = drawTileAt r c & drawCols r (c+1)

main :: IO ()
main = drawingOf pictureOfMaze
