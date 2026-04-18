data Expr = Const Double
    | Var
    |Add Expr Expr
    |Sub Expr Expr
    |Mul Expr Expr
    |Div Expr Expr
    -- |Pow Expr Expr

eval :: Expr -> Double -> Double
eval (Const c) _ = c
eval Var       x = x
eval (Add a b) x = eval a x + eval b x
eval (Sub a b) x = eval a x - eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Div a b) x = eval a x / eval b x

diff :: Expr -> Expr --求导
diff (Const _) = Const 0
diff Var       = Const 1
diff (Add a b) = Add (diff a) (diff b)
diff (Sub a b) = Sub (diff a) (diff b)
diff (Mul a b) = Add (Mul (diff a) b) (Mul a (diff b))
diff (Div a b) = Div (Sub (Mul (diff a) b) (Mul a (diff b))) (Mul b b)
-- diff (Pow a (Const n)) = Mul (Mul (Const n) (Pow a (Const (n-1)))) (diff a)

--n牛顿
newton :: Expr -> Expr -> Double -> Double -> Double -> Maybe Double
--函数，导函数，猜测值，误差，迭代num，根
newton expr exprDao x0 wc maxn = loop x0 0
    where
        loop x n
            | n >= maxn = Nothing
            | abs (eval expr x) < maxn = Just x
            | otherwise = 
                let fx = eval expr x
                    fdaox = eval exprDao x
                in if abs fdaox < 1e-12
                    then Nothing --div 0
                    else let x1 = x - fx / fdaox
                        in if abs (x1 - x) < maxn
                            then Just x1
                            else loop x1
            
main :: IO ()
main = do
    --x^2 - 2 = 0
    let f = Sub (Mul Var Var) (Const 2)
        f' = diff f
        result = newton f f' 1.0 1e-8 100
    case result of
        Just root -> print root
        Nothing   -> putStrLn "牛顿法未收敛"
