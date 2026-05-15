module Imag where

data Complex = Complex Double Double

re :: Complex -> Double
re (Complex r _) = r

im :: Complex -> Double
im (Complex _ i) = i

add :: Complex -> Complex -> Complex
add (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

mul :: Complex -> Complex -> Complex
mul (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2)

--instance Show Complex where
--    show (Complex r i) = show r ++ " + " ++ show i ++ "i"
