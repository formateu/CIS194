import CodeWorld


drawColoredSquare :: Color -> Picture
drawColoredSquare color = colored color (solidRectangle 1 1)

wall = drawColoredSquare gray
ground = drawColoredSquare yellow
storage = colored black (solidCircle 0.25) & ground
box = drawColoredSquare brown

maze :: Double -> Double -> Picture
maze x y
  | abs x > 4  || abs y > 4  = blank
  | abs x == 4 || abs y == 4 = wall
  | x ==  2 && y <= 0        = wall
  | x ==  3 && y <= 0        = storage
  | x >= -2 && y == 0        = box
  | otherwise                = ground
  
axis :: [Double]  
axis = [-10..10]

cartProd xs ys = [(x,y) | x <- xs, y <- ys]
matrix = cartProd axis axis

pictureOfMaze :: Picture
pictureOfMaze = foldl1 (&) (fmap (\(x,y) -> translated x y (maze x y)) matrix)

main = drawingOf pictureOfMaze