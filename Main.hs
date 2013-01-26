module Main where

import Data.Maybe
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Environment

import SExpr

isolateErrs :: SExpr -> (Maybe SExpr, [ParseError])
isolateErrs (SEError e) = (Nothing, [e])
isolateErrs (SEList l body) = let results = map isolateErrs body in
                              (Just (SEList l (mapMaybe fst results)), concatMap snd results)
isolateErrs x = (Just x, [])

main :: IO ()
main = do
  as <- getArgs
  case as of
    (file:_) -> do input <- fmap E.decodeUtf8 (B.readFile file)
                   let results = map isolateErrs $ parseAll input
                   print (mapMaybe fst results)
                   mapM_ (B.putStr . E.encodeUtf8 . (`T.append` T.singleton '\n') .
                           prettyError (T.pack file) input)
                             (concatMap snd results)
    [] -> B.interact (E.encodeUtf8 . T.pack . show . parseAll . E.decodeUtf8)