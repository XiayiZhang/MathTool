module Solving where

import Text.Parsec
import Text.Parsec.String (Parser) 

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

-- 牛顿法
newton :: Expr -> Expr -> Double -> Double -> Int -> Maybe Double
-- 函数，导函数，猜测值，误差，最大迭代数，根
newton expr exprDao x0 wc maxn = loop x0 0
    where
        loop x n
            | n >= maxn = Nothing
            | abs (eval expr x) < wc = Just x
            | otherwise = 
                let fx = eval expr x
                    fdaox = eval exprDao x
                in if abs fdaox < 1e-12
                    then Nothing -- div 0
                    else let x1 = x - fx / fdaox
                        in if abs (x1 - x) < wc
                            then Just x1
                            else loop x1 (n + 1)

-- 解析字符串为 Expr
parseExpr :: String -> Maybe Expr
parseExpr input = case parse exprParser "" input of
    Right e -> Just e
    Left _ -> Nothing

exprParser :: Parser Expr
exprParser = do
    spaces
    e <- addSub
    eof
    return e

addSub :: Parser Expr
addSub = chainl1 mulDiv (try (char '+' >> return Add) <|> try (char '-' >> return Sub))

mulDiv :: Parser Expr
mulDiv = chainl1 factor (try (char '*' >> return Mul) <|> try (char '/' >> return Div))

factor :: Parser Expr
factor = try (do
    char '('
    spaces
    e <- addSub
    spaces
    char ')'
    return e
    ) <|> try (do
    string "x"
    return Var
    ) <|> try (do
    n <- many1 digit
    return (Const (read n))
    )