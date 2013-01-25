module SExpr where

import qualified Data.Text as T
import Data.List
import Data.Char
import Numeric

data Atom = SESymbol T.Text | SEInteger Integer | SEDecimal Rational | SEString T.Text

instance Show Atom where
  show (SESymbol n) = show n
  show (SEInteger i) = show i
  show (SEDecimal d) = show d
  show (SEString s) = '"' : T.unpack s ++ "\""

data SourceLoc = SourceLoc Int Int Int
data SourceRange = SourceRange SourceLoc SourceLoc

instance Show SourceLoc where
  show (SourceLoc line col _) = show line ++ ":" ++ show col

instance Show SourceRange where
  show (SourceRange start end) = show start ++ " - " ++ show end

newline :: SourceLoc -> SourceLoc
newline (SourceLoc line _ char) = SourceLoc (line+1) 0 (char+1)

newchar :: SourceLoc -> SourceLoc
newchar (SourceLoc line col char) = SourceLoc line (col+1) (char+1)

data SExpr = SEList SourceRange [SExpr] | SEAtom SourceRange Atom | SEError ParseError

instance Show SExpr where
  show (SEList _ xs) = '(' : intercalate "," (map show xs) ++ ")"
  show (SEAtom _ atom) = show atom
  show (SEError e) = show e

data ParseError = ParseError SourceRange String

instance Show ParseError where
  show (ParseError range string) = show range ++ ": " ++ string

data Token = TLBracket SourceLoc | TRBracket SourceLoc | TAtom SourceRange Atom
           | TError ParseError

instance Show Token where
  show (TLBracket _) = "("
  show (TRBracket _) = ")"
  show (TAtom _ a) = show a
  show (TError err) = show err
  
parse :: [Token] -> (SExpr, SourceLoc, [Token])
parse [] = (SEError (ParseError (SourceRange (SourceLoc 0 0 0) (SourceLoc 0 0 0)) "Empty input"), SourceLoc 0 0 0, [])
parse (TError e@(ParseError (SourceRange _ end) _) : xs) = (SEError e, end, xs)
parse (TAtom range@(SourceRange _ end) atom : xs) = (SEAtom range atom, end, xs)
parse (TLBracket start : xs) = let (body, range@(SourceRange _ end), unparsed) =
                                     takeList (SourceRange start start) xs [] in
  (SEList range body, end, unparsed)
parse (TRBracket end : xs) = (SEError (ParseError (SourceRange end end) "Orphaned close parenthesis"), end, xs)

takeList :: SourceRange -> [Token] -> [SExpr] -> ([SExpr], SourceRange, [Token])
takeList (SourceRange start _) (TRBracket end : xs) accum = (reverse accum, (SourceRange start end), xs)
takeList (SourceRange start _) xs@(_:_) accum = let (expr, end, unparsed) = parse xs in
                                          takeList (SourceRange start end) unparsed (expr:accum)
takeList range [] accum =
  (reverse $ SEError (ParseError range "Unterminated list"):accum
  , undefined, [])

buildRational :: [Char] -> [Char] -> [Char] -> Rational
buildRational whole fractional power =
  let w = read whole :: Integer
      f = if null fractional then 0 else read fractional :: Integer
      e = if null power then 0 else read power :: Integer in
  (fromInteger w + fromInteger f / 10 ^ (length fractional)) * 10 ^ e

data StringMode = Normal
                | Escaping SourceLoc
                | ScalarValue SourceLoc Bool [Char] -- bool is long flag
                deriving Show

data TokState = STNil
              | STString SourceLoc StringMode [ParseError] [Char]
              | STSymbol SourceLoc [Char]
              | STInteger SourceLoc [Char]
              | STDecimal SourceLoc [Char] [Char] -- whole fractional
              | STExponential SourceLoc [Char] [Char] [Char] -- whole fractional power
              deriving Show

data ParseState = ParseState { tokAccum :: [Token]
                             , tokState :: TokState
                             , lastLoc :: SourceLoc
                             }
                deriving Show

initialState :: ParseState
initialState = ParseState { tokAccum = []
                          , tokState = STNil
                          , lastLoc = SourceLoc 0 0 0
                          }

pushTok :: ParseState -> Token -> ParseState
pushTok s t = s { tokState = STNil, tokAccum = t : tokAccum s }

parseTok :: ParseState -> TokState -> ParseState
parseTok s t = s { tokState = t }

-- Finish the accumulated token
finishTok :: ParseState -> ParseState
finishTok st =
  let here = lastLoc st in
  foldl pushTok st $
  case tokState st of
    STNil -> []
    STString start Normal errors accum ->
      (TAtom (SourceRange start here) (SEString (T.pack (reverse accum))))
      : map TError errors
    STString start _ errors _ ->
      (TError (ParseError (SourceRange start here) "Incomplete escape sequence"))
      : map TError errors
    STSymbol start accum ->
      [TAtom (SourceRange start here) (SESymbol (T.pack (reverse accum)))]
    STInteger start accum ->
      [TAtom (SourceRange start here) (SEInteger (read (reverse accum) :: Integer))]
    STDecimal start whole fractional ->
      [TAtom (SourceRange start here)
       (SEDecimal (buildRational
                   (reverse whole)
                   (reverse fractional) []))]
    STExponential start whole fractional power ->
      [TAtom (SourceRange start here)
       (SEDecimal (buildRational
                   (reverse whole)
                   (reverse fractional)
                   (reverse power)))]

tokenize :: T.Text -> [Token]
tokenize text = let st = T.foldl step initialState text in
  case tokState st of
    STString start _ _ _ -> reverse (TError (ParseError (SourceRange start (lastLoc st))
                                             "Incomplete string literal") : tokAccum (finishTok st))
    _ -> reverse . tokAccum . finishTok $ st

isScalarValue :: Integer -> Bool
isScalarValue x = x >= 0 && x < 0xD800
                  || x >= 0xE000 && x < 0x110000

step :: ParseState -> Char -> ParseState
step st c =
  let here = if c == '\n' then newline (lastLoc st) else newchar (lastLoc st)
      st' = st { lastLoc = here }
      continue = parseTok st'
      finished = finishTok st' in
  case (tokState st) of
    STNil ->
      case c of
        '(' -> pushTok st' (TLBracket here)
        ')' -> pushTok st' (TRBracket here)
        '"' -> continue (STString here Normal [] [])
        '-' -> continue (STInteger here [c])
        _ -> if isSpace c then st'
             else if isDigit c then continue (STInteger here [c])
                  else continue (STSymbol here [c])
    STString start Normal errors accum ->
      case c of
        '"' -> finished
        '\\' -> continue (STString start (Escaping here) errors accum)
        _ -> continue (STString start Normal [] (c:accum))
    STString start (Escaping escStart) errors accum ->
      let char x = continue (STString start Normal errors (x:accum)) in
      case c of
        'r' -> char '\r'
        'n' -> char '\n'
        't' -> char '\t'
        'u' -> continue (STString start (ScalarValue (newchar here) False []) errors accum)
        'U' -> continue (STString start (ScalarValue (newchar here) True []) errors accum)
        _ -> continue (STString start Normal
                       ((ParseError (SourceRange escStart here) "Unrecognized escape sequence"):errors)
                       accum)
    STString start (ScalarValue escStart isLong valDigits) errors accum ->
      if length valDigits < (if isLong then 8 else 4) && isHexDigit c
      then continue (STString start (ScalarValue escStart isLong (c:valDigits)) errors accum)
      else let [(value, _)] = (readHex (reverse valDigits)) in
           if isScalarValue value then step (continue $
                                             STString start Normal errors (chr (fromInteger value):accum)) c
           else step (continue $ STString start Normal
                      (ParseError (SourceRange escStart here) "Invalid Unicode scalar value" : errors)
                      accum) c
    STSymbol start accum ->
      let done = step finished c in
      case c of
        '(' -> done
        ')' -> done
        _ -> if isSpace c then done
             else continue (STSymbol start (c:accum))
    STInteger start accum ->
      let done = step finished c in
      case c of
        '(' -> done
        ')' -> done
        '.' -> continue (STDecimal start accum [])
        'e' -> continue (STExponential start accum [] [])
        _ -> if isDigit c then continue (STInteger start (c:accum))
             else if isSpace c then done
                  else continue (STSymbol start (c:accum))
    STDecimal start whole fractional ->
      let done = step finished c in
      case c of
        '(' -> done
        ')' -> done
        'e' -> continue (STExponential start whole fractional [])
        _ -> if isDigit c then continue (STDecimal start whole (c:fractional))
             else if isSpace c then done
                  else continue (STSymbol start (c:fractional ++ "." ++ whole))
    STExponential start whole fractional power ->
      let done = step finished c in
      case c of
        '(' -> done
        ')' -> done
        _ -> if isDigit c then continue (STExponential start whole fractional (c:power))
             else continue (STSymbol start (c:power ++ "e" ++ fractional ++ "." ++ whole))
