module Main where

import qualified MyLib (someFunc)
import qualified Tiling (hello)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn Tiling.hello
  MyLib.someFunc
