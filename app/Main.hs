module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let usage = "Usage: readme (JWT|Cookie)"
  case args of
    ["Cookie"] -> mainWithCookies
    e -> putStrLn $ "Arguments: \"" ++ unwords e ++ "\" not understood\n" ++ usage
