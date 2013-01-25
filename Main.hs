module Main where

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Environment

import SExpr

main :: IO ()
main = do
  as <- getArgs
  case as of
    (file:_) -> fmap (parseAll . E.decodeUtf8) (B.readFile file) >>= print
    [] -> B.interact (E.encodeUtf8 . T.pack . show . parseAll . E.decodeUtf8)
