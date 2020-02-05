{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

blossom :: Double -> Picture
blossom size = colored yellow (solidCircle size)

tree :: Integer -> Double -> Picture
tree 0 size = colored yellow (blossom size)
tree n size = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) size) & rotated (- pi/10) (tree (n-1) size))

blossomingTree :: Double -> Picture
blossomingTree t
  | t >= 0 && t < 10 = tree 8 (t / 30)
  | otherwise = tree 8 0.34

  
main :: IO ()
main = animationOf blossomingTree