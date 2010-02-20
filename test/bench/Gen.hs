import System.Environment
import Data.List

field i n = "x" ++ show i ++ ": T" ++ show n
fields n = intercalate "," [field i n | i <- [1..10]]
constr i n = "C" ++ show i ++ "_" ++ show n ++ "(" ++ fields n ++ ")"
constrs n = intercalate "\n|" [constr i n | i <- [1..10]]
ty n = "T" ++ show n ++ " = " ++ constrs n
tys n = intercalate "\n" [ty i | i <- [1..n]]
modul n = "module big\nabstract syntax\n" ++ tys n

main = do [n] <- getArgs
          putStrLn $ modul (read n)
