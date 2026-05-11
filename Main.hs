import Text.Read (readMaybe)
import Solving (Expr(..), eval, diff, newton, parseExpr)
import StringFunc (findSubstr, findBetween)
--import Plot (drawPlot)

main :: IO ()
main = do
    putStrLn "请输入表达式"
    a <- getLine
    case parseExpr a of
        Just f -> do
            putStrLn $ "func graph of y = " ++ a
            --mapM_ putStrLn (drawPlot f (-10) 10)
            print $ "root of " ++ a ++ " = 0"
            let f' = diff f
            let result = newton f f' 1.0 1e-8 100
            case result of
                Just root -> print root
                Nothing   -> putStrLn "牛顿法未收敛"
        Nothing -> putStrLn "表达式解析失败"
            