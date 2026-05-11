module Plot where

import Data.List (foldl')
import Solving (Expr(..), eval)

width, height :: Int
width = 80
height = 80

-- Generate an 80x80 ASCII plot for f over [x0, x1].
{-慢慢看吧
drawPlot :: Expr -> Double -> Double -> [String]
drawPlot f x0 x1 = grid2
  where
    xs = [x0 + dx * fromIntegral i | i <- [0 .. width - 1]]
    dx = (x1 - x0) / fromIntegral (width - 1)
    ys = map eval f xs
    finiteSamples = [ (i, y) | (i, y) <- zip [0 ..] ys, isFinite y ]
    isFinite y = not (isNaN y || isInfinite y)
    (yMin, yMax)
      | null finiteSamples = (-1, 1)
      | otherwise = let ys' = map snd finiteSamples
                        low = minimum ys'
                        high = maximum ys'
                    in if low == high then (low - 1, high + 1) else (low, high)
    rowOf y = clamp 0 (height - 1) . round $ (yMax - y) / (yMax - yMin) * fromIntegral (height - 1)
    xAxisRow = if yMin <= 0 && 0 <= yMax then Just (rowOf 0) else Nothing
    yAxisCol = if x0 <= 0 && 0 <= x1 then Just (round ((0 - x0) / dx)) else Nothing
    emptyGrid = replicate height (replicate width ' ')
    gridWithAxes = case (xAxisRow, yAxisCol) of
      (Just r, Just c) -> setChar (setAxisX emptyGrid r) r c '+'  -- origin after both axes
      (Just r, Nothing) -> setAxisX emptyGrid r
      (Nothing, Just c) -> setAxisY emptyGrid c
      _ -> emptyGrid
    grid2 = foldl' placePoint gridWithAxes [ (i, rowOf y) | (i, y) <- finiteSamples ]

    setAxisX g r = foldl' (\tg c -> setChar tg r c '-') g [0 .. width - 1]
    setAxisY g c = foldl' (\tg r -> setChar tg r c '|') g [0 .. height - 1]
    placePoint g (c, r) = setChar g r c '*'

    setChar g r c ch =
      take r g ++ [row'] ++ drop (r + 1) g
      where
        row = g !! r
        row' = take c row ++ [ch] ++ drop (c + 1) row

    clamp mn mx v
      | v < mn = mn
      | v > mx = mx
      | otherwise = v
-}