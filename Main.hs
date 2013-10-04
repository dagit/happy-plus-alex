module Main(main) where

import Language ( eval )
import Parser   ( parseExp )

main :: IO ()
main = repl

repl :: IO a
repl = do
  cs <- getContents
  case parseExp cs of
    Left  e -> putStrLn ("error: " ++ e) >> repl
    Right e -> print (eval [] e) >> repl
    
