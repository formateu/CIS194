import CodeWorld

tree :: Integer -> Double -> Picture
tree 0 s = colored yellow (solidCircle s)
tree n s = polyline [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/10) (tree (n-1) s) & rotated (- pi/10) (tree (n-1) s))


animatedTree :: Double -> Picture
animatedTree t
  | t >=0 && t <= 10 = tree 8 (t/20)
  | otherwise       = tree 8 0.5

main = animationOf animatedTree
