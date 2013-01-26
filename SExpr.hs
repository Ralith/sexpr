{-# LANGUAGE OverloadedStrings #-}
module SExpr where

import qualified Data.Text as T
import Data.Ratio
import Data.Char
import Numeric

data Atom = SESymbol T.Text | SEInteger Integer | SERational Rational | SEChar T.Text | SEString T.Text | SEComment T.Text

instance Show Atom where
  show (SESymbol n) = T.unpack n
  show (SEChar c) = "#\\" ++ T.unpack c
  show (SEInteger i) = show i
  show (SERational r) = show r
  show (SEString s) = '"' : T.unpack s ++ "\""
  show (SEComment c) = "#|" ++ T.unpack c ++ "|#"

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
  show (SEList _ xs) = '(' : unwords (map show xs) ++ ")"
  show (SEAtom _ atom) = show atom
  show (SEError e) = show e

data ParseError = ParseError SourceRange T.Text

instance Show ParseError where
  show (ParseError range message) = "#<" ++ show range ++ ": " ++ T.unpack message ++ ">"

prettyError :: T.Text -> T.Text -> ParseError -> T.Text
prettyError file source (ParseError r@(SourceRange (SourceLoc lineA colA _) (SourceLoc lineB colB _)) message)
    | lineA == lineB =
        T.concat [ file, ":", T.pack (show lineA ++ ":" ++ show colA ++ "-" ++ show colB ++ ": ")
                 , message, "\n"
                 , T.splitOn "\n"  source !! (lineA - 1), "\n"
                 , T.replicate (colA - 1) " ", T.replicate (colB - colA) "~"
                 ]
    | otherwise =
        let body@(lineOne:_) = take (lineB - lineA + 1) (drop (lineA - 1) (T.splitOn "\n" source)) in
        T.concat $ [ file, ":", T.pack (show r), ": ", message, "\n"
                   , T.replicate (colA - 1) " ", "V"
                   , T.replicate (T.length lineOne - colA) "~", "\n"
                   , T.intercalate "\n" body, "\n"
                   , T.replicate (colB - 1) "~", "^"
                   ]

data Token = TLBracket SourceLoc | TRBracket SourceLoc | TAtom SourceRange Atom
           | TError ParseError

instance Show Token where
  show (TLBracket _) = "("
  show (TRBracket _) = ")"
  show (TAtom _ a) = show a
  show (TError err) = show err
  
-- Returns a single s-sexpression, the location at which parsing completed, and any remaining, unparsed tokens
parse :: [Token] -> (SExpr, SourceLoc, [Token])
parse [] = (SEError (ParseError (SourceRange (SourceLoc 1 1 0) (SourceLoc 1 1 0)) "Empty input"), SourceLoc 1 1 0, [])
parse (TError e@(ParseError (SourceRange _ end) _) : xs) = (SEError e, end, xs)
parse (TAtom range@(SourceRange _ end) atom : xs) = (SEAtom range atom, end, xs)
parse (TLBracket start : xs) = let (body, range@(SourceRange _ end), unparsed) =
                                     takeList (SourceRange start start) xs [] in
  (SEList range body, end, unparsed)
parse (TRBracket end : xs) = (SEError (ParseError (SourceRange end end) "Orphaned close parenthesis"), end, xs)

takeList :: SourceRange -> [Token] -> [SExpr] -> ([SExpr], SourceRange, [Token])
takeList (SourceRange start _) (TRBracket end : xs) accum = (reverse accum, SourceRange start end, xs)
takeList (SourceRange start _) xs@(_:_) accum = let (expr, end, unparsed) = parse xs in
                                          takeList (SourceRange start end) unparsed (expr:accum)
takeList range [] accum =
  (reverse $ SEError (ParseError range "Unterminated list"):accum
  , range, [])

buildRational :: String -> String -> String -> Rational
buildRational whole fractional power =
  let w = read whole :: Integer
      f = if null fractional then 0 else read fractional :: Integer
      e = if null power then 0 else read power :: Integer in
  (fromInteger w + fromInteger f / 10 ^ length fractional) * 10 ^ e

data StringMode = StrNormal
                | StrEscaping SourceLoc
                | StrScalarValue SourceLoc Bool String -- bool is long flag
                deriving Show

data TokState = STNil
              | STString SourceLoc StringMode [ParseError] String
              | STSymbol SourceLoc String
              | STInteger SourceLoc String
              | STDecimal SourceLoc String String -- whole fractional
              | STExponential SourceLoc String String String -- whole fractional power
              | STRatio SourceLoc String String -- num denom
              | STLineComment SourceLoc String
              | STBlockComment SourceLoc Integer Bool String -- integer is depth, bool is whether last # was a terminator
              | STHash SourceLoc
              | STChar SourceLoc String
              deriving Show

data ParseState = ParseState { tokAccum :: [Token]
                             , tokState :: TokState
                             , currentLoc :: SourceLoc
                             , lastLoc :: SourceLoc
                             }
                deriving Show

initialState :: ParseState
initialState = ParseState { tokAccum = []
                          , tokState = STNil
                          , currentLoc = SourceLoc 1 1 0
                          , lastLoc = SourceLoc 1 0 0
                          }

pushTok :: ParseState -> Token -> ParseState
pushTok s t = s { tokState = STNil, tokAccum = t : tokAccum s }

parseTok :: ParseState -> TokState -> ParseState
parseTok s t = s { tokState = t }

-- Finish the accumulated token
finishTok :: ParseState -> ParseState
finishTok st =
  let here = currentLoc st in
  foldl pushTok st $
  case tokState st of
    STNil -> []
    STHash start -> [TError (ParseError (SourceRange start (newchar start)) "Orphaned hash")]
    STString start StrNormal errors accum ->
      TAtom (SourceRange start here) (SEString (T.pack (reverse accum)))
      : map TError errors
    STString start _ errors _ ->
      TError (ParseError (SourceRange start here) "Incomplete escape sequence")
      : map TError errors
    STSymbol start accum ->
      [TAtom (SourceRange start here) (SESymbol (T.pack (reverse accum)))]
    STChar start accum ->
      [TAtom (SourceRange start here) (SEChar (T.pack (reverse accum)))]
    STInteger start accum ->
      [TAtom (SourceRange start here) (SEInteger (read (reverse accum)))]
    STDecimal start whole fractional ->
      [TAtom (SourceRange start here)
       (SERational (buildRational
                    (reverse whole)
                    (reverse fractional) []))]
    STExponential start whole fractional power ->
      [TAtom (SourceRange start here)
       (SERational (buildRational
                    (reverse whole)
                    (reverse fractional)
                    (reverse power)))]
    STRatio start _ [] ->
        [TError (ParseError (SourceRange start here) "Incomplete ratio")]
    STRatio start num denom ->
      [TAtom (SourceRange start here)
       (SERational (read (reverse num) % read (reverse denom)))]
    STLineComment start accum ->
        [TAtom (SourceRange start here) (SEComment (T.pack (reverse accum)))]
    STBlockComment start _ _ accum ->
        [TAtom (SourceRange start here) (SEComment (T.pack (reverse accum)))]

incomplete :: ParseState -> Maybe ParseError
incomplete st =
    case tokState st of
      STString start _ _ _ ->
          Just $ ParseError (SourceRange start (currentLoc st)) "Incomplete string literal"
      STBlockComment start _ _ _ ->
          Just $ ParseError (SourceRange start (currentLoc st)) "Incomplete block comment"
      _ -> Nothing

-- Tokenize a complete set of textual s-expressions
tokenize :: T.Text -> [Token]
tokenize text = let st = T.foldl step initialState text in
                case incomplete st of
                  Just e -> reverse (TError e : (tokAccum . finishTok $ st))
                  Nothing -> reverse . tokAccum . finishTok $ st

parseAll :: T.Text -> [SExpr]
parseAll text = helper (tokenize text)
    where
      helper :: [Token] -> [SExpr]
      helper [] = []
      helper ts = let (e, _, ts') = parse ts in e : helper ts'

isScalarValue :: Integer -> Bool
isScalarValue x = x >= 0 && x < 0xD800
                  || x >= 0xE000 && x < 0x110000

breaksTok :: Char -> Bool
breaksTok c = isSpace c || elem c others
    where
      others = "();#"

updateLoc :: ParseState -> Char -> ParseState
updateLoc st '\n' = st { lastLoc = currentLoc st, currentLoc = newline (currentLoc st) }
updateLoc st _    = st { lastLoc = currentLoc st, currentLoc = newchar (currentLoc st) }

step :: ParseState -> Char -> ParseState
step st c = updateLoc (step' st c) c

step' :: ParseState -> Char -> ParseState
step' st c =
  let here = currentLoc st
      prev = lastLoc st
      continue = parseTok st
      finished = finishTok st in
  case tokState st of
    STNil
        | isSpace c -> st
        | isDigit c -> continue (STInteger here [c])
        | otherwise ->
            case c of
              '(' -> pushTok st (TLBracket here)
              ')' -> pushTok st (TRBracket here)
              '"' -> continue (STString here StrNormal [] [])
              '-' -> continue (STInteger here [c])
              ';' -> continue (STLineComment here [])
              '#' -> continue (STHash here)
              _   -> continue (STSymbol here [c])
    STHash start
        | c == '\\' -> continue (STChar here [])
        | c == '|' -> continue (STBlockComment here 0 False [])
        | otherwise ->
            pushTok st (TError (ParseError (SourceRange start here) "Unrecognized hash dispatch character"))
    STLineComment start accum
        | c == '\n' -> finished
        | otherwise -> continue (STLineComment start (c:accum))
    STBlockComment start 0 _ ('|':accum) | c == '#' ->
      -- We don't use the finisher here so we can cleanly prevent a trailing | from appearing
      pushTok st (TAtom (SourceRange start here) (SEComment (T.pack (reverse accum))))
    STBlockComment start depth _ accum@('|':_) | c == '#' ->
      continue (STBlockComment start (depth-1) True (c:accum))
    STBlockComment start depth False accum@('#':_) | c == '|' ->
      continue (STBlockComment start (depth+1) False (c:accum))
    STBlockComment start depth _ accum ->
      continue (STBlockComment start depth False (c:accum))
    STChar start accum
        | breaksTok c -> step' finished c
        | otherwise -> continue (STChar start (c:accum))
    STString start StrNormal errors accum
        | c == '"' -> finished
        | c == '\\' -> continue (STString start (StrEscaping here) errors accum)
        | otherwise -> continue (STString start StrNormal errors (c:accum))
    STString start (StrEscaping escStart) errors accum ->
      let char x = continue (STString start StrNormal errors (x:accum)) in
      case c of
        'r' -> char '\r'
        'n' -> char '\n'
        't' -> char '\t'
        'u' -> continue (STString start (StrScalarValue here False []) errors accum)
        'U' -> continue (STString start (StrScalarValue here True []) errors accum)
        _ -> continue (STString start StrNormal
                       (ParseError (SourceRange escStart (newchar here)) "Unrecognized escape sequence":errors)
                       accum)
    STString start (StrScalarValue escStart isLong valDigits) errors accum
        | length valDigits < (if isLong then 8 else 4) && isHexDigit c ->
           continue (STString start (StrScalarValue escStart isLong (c:valDigits)) errors accum)
        | null valDigits ->
            step' (continue $ STString start StrNormal
                   (ParseError (SourceRange escStart here) "Missing Unicode scalar value" : errors)
                   accum) c
        | otherwise ->
            let [(value, _)] = (readHex (reverse valDigits)) in
            if isScalarValue value
            then step' (continue $ STString start StrNormal errors (chr (fromInteger value):accum)) c
            else step' (continue $ STString start StrNormal
                       (ParseError (SourceRange escStart here) "Invalid Unicode scalar value" : errors)
                        accum) c
    STSymbol start accum
        | breaksTok c -> step' finished c
        | otherwise -> continue (STSymbol start (c:accum))
    STInteger start accum
        | c == '.' -> continue (STDecimal start accum [])
        | c == 'e' -> continue (STExponential start accum [] [])
        | c == '/' -> continue (STRatio start accum [])
        | isDigit c -> continue (STInteger start (c:accum))
        | breaksTok c -> step' finished c
        | otherwise -> continue (STSymbol start (c:accum))
    STDecimal start whole fractional
        | c == 'e' -> continue (STExponential start whole fractional [])
        | isDigit c -> continue (STDecimal start whole (c:fractional))
        | breaksTok c -> step' finished c
        | otherwise -> continue (STSymbol start (c:fractional ++ "." ++ whole))
    STExponential start whole fractional power
        | isDigit c -> continue (STExponential start whole fractional (c:power))
        | breaksTok c -> step' finished c
        | otherwise -> continue (STSymbol start (c:power ++ "e" ++ fractional ++ "." ++ whole))
    STRatio start num denom
        | isDigit c -> continue (STRatio start num (c:denom))
        | breaksTok c -> step' finished c
        | otherwise -> continue (STSymbol start (c:denom ++ "/" ++ num))
