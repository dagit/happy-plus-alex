module Main(main) where

import Language ( eval )
import Parser ( parseExp )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parseExp "<stdin>") getContents
              [f] -> fmap (parseExp f) (readFile f)
              _   -> error "expected max. 1 argument"
  either putStrLn (print . eval []) result
