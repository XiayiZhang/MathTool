import Text.Read (readMaybe)


            
main :: IO ()
main = do
    --x^2 - 2 = 0
    --let f = Sub (Mul Var Var) (Const 2)
    --    f' = diff f
    --    result = newton f f' 1.0 1e-8 100
    --case result of
    --     Just root -> print root
    --    Nothing   -> putStrLn "牛顿法未收敛"
    print "请输入表达式"
    a <- getLine
    case readMaybe a :: Maybe Double of
        Just x -> do
