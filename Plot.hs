module Plot where

import Solving (Expr(..), eval, diff, newton, parseExpr)

drawFunc :: Expr -> Double -> Double -> [Int]
drawFunc f x0 x1 = loop x0 []
    where
        loop x acc
            | x > x1    = acc
            | otherwise = loop (x + 1) (acc ++ [round (eval f x) ])

printFunc :: [Int] -> IO ()
printFunc n = loop 1 ""
  where
    max = maximum n
    min = minimum n
    loop i a
      | i >= length n     = putStrLn a
      | otherwise = do
          let b = replicate ( n !! i - min) ' '
          loop (i+1) (a ++ b ++ "*\n")