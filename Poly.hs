module Poly where

import Solving (Expr(..), eval)

type Dishu = Double
type Zhishu = Int
type Term = (Dishu, Zhishu)

type Dxs = [Dishu]

zhuanhuan :: Expr -> Dxs
zhuanhuan (Add a b) = addP (zhuanhuan a) (zhuanhuan b)
zhuanhuan (Sub a b) = subP (zhuanhuan a) (zhuanhuan b)
zhuanhuan (Mul a b) = mulP (zhuanhuan a) (zhuanhuan b)

addP :: Dxs -> Dxs -> Dxs
addP = go 0
  where
    go _ [] [] = []
    go i (x:xs) (y:ys) = (x+y) : go (i+1) xs ys
    go i xs [] = xs
    go i [] ys = ys

subP :: Dxs -> Dxs -> Dxs
subP p1 p2 = addP p1 (map negate p2)

mulP :: Dxs -> Dxs -> Dxs --卷积
mulP p1 p2 = foldr addPoly [] [ replicate i 0 ++ map (c*) p2 | (i,c) <- zip [0..] p1 ]