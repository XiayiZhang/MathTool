import Text.Read (readMaybe)
import Solving (Expr(..), eval, diff, newton, parseExpr)
import StringFunc (findSubstr, findBetween)
import Plot (drawFunc, printFunc)

            
main :: IO ()
main = do
    putStrLn "请输入表达式"
    a <- getLine
    case parseExpr a of
        Just f -> do
            --putStrLn $ show (drawFunc f (-10) 10)
            print "func graph: "
            printFunc (drawFunc f (-10) 10)
            print "root: "
            let f' = diff f
            let result = newton f f' 1.0 1e-8 100
            case result of
                Just root -> print root
                Nothing   -> putStrLn "牛顿法未收敛"
        Nothing -> putStrLn "表达式解析失败"
            