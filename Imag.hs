data Complex = Complex Double Double

-- 构造函数
real :: Complex -> Double
real (Complex r _) = r

imag :: Complex -> Double
imag (Complex _ i) = i

-- 加法
add :: Complex -> Complex -> Complex
add (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

-- 乘法
mul :: Complex -> Complex -> Complex
mul (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2)

-- 显示
instance Show Complex where
    show (Complex r i) = show r ++ " + " ++ show i ++ "i"

-- 示例用法
let z1 = Complex 1 2  -- 1 + 2i
let z2 = Complex 3 4  -- 3 + 4i
let sum = add z1 z2   -- 4 + 6i