module Main where

import Data.Text.Encoding as E
import qualified Data.ByteString as B
import System.Environment

import SExpr

main :: IO ()
main = do
  as <- getArgs
  case as of
    (file:_) -> fmap (parseAll . E.decodeUtf8) (B.readFile file) >>= print
    [] -> putStr "<filename>"