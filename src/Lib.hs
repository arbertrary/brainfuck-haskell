{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( ValidationResult(TooManyClosed, TooManyOpen, Fine, NoCode)
  , validateBrackets
  , cleanupCode
  , interpretCode
  , Tape(..)
  -- Test exports
  , emptyTape
  , moveLeft
  , moveRight
  , increment
  , decrement
  , readChar
  , writeChar
  , handleChar
  , partitionByFinding
  ) where

import Control.Lens
import Data.Char (chr, ord)
import Data.List (intercalate, intersperse)

data Tape = Tape
  { _left :: [Int]
  , _curr :: Int
  , _right :: [Int]
  } deriving (Eq)

makeLenses ''Tape

instance Show Tape where
  show (Tape l c r) = show $ "[" ++ l' ++ "|>>" ++ show c ++ "<<|" ++ r' ++ "]"
    where
      l' = intersperse '|' $ intercalate "" $ show <$> reverse l
      r' = intersperse '|' $ intercalate "" $ show <$> r

emptyTape :: Tape
emptyTape = Tape [] 0 []

moveLeft :: Tape -> Tape
moveLeft t@Tape {_left = [], _curr = rh, _right = r} =
  t & left .~ [] & curr .~ 0 & right .~ (rh : r)
moveLeft t@Tape {_left = c:l, _curr = rh, _right = r} =
  t & left .~ l & curr .~ c & right .~ rh : r

moveRight :: Tape -> Tape
moveRight t@Tape {_left = l, _curr = lh, _right = []} =
  t & left .~ (lh : l) & curr .~ 0 & right .~ []
moveRight t@Tape {_left = l, _curr = lh, _right = c:r} =
  t & left .~ (lh : l) & curr .~ c & right .~ r

increment :: Tape -> Tape
increment t = t & curr .~ incrWithOverflow (t ^. curr)
  where
    incrWithOverflow i =
      if i == 255
        then 0
        else i + 1

decrement :: Tape -> Tape
decrement t = t & curr .~ decrWithOverflow (t ^. curr)
  where
    decrWithOverflow i =
      if i == 0
        then 255
        else i - 1

readChar :: Tape -> Char
readChar tape = chr $ tape ^. curr

writeChar :: Tape -> Char -> Tape
writeChar tape c = tape & curr .~ ord c

data ValidationResult
  = TooManyOpen
  | TooManyClosed
  | Fine
  | NoCode
  deriving (Eq, Show)

handleChar :: Char -> Tape -> Tape
handleChar '>' = moveRight
handleChar '<' = moveLeft
handleChar '+' = increment
handleChar '-' = decrement
handleChar other = error $ "Unexpected invalid character: " ++ [other]

cleanupCode :: String -> String
cleanupCode = filter (`elem` validChars)
  where
    validChars = "<>[],.+-"

validateBrackets :: String -> ValidationResult
validateBrackets s
  | null s = NoCode
  | count > 0 = TooManyOpen
  | count < 0 = TooManyClosed
  | otherwise = Fine
  where
    aggr sum '[' = sum + 1
    aggr sum ']' = sum - 1
    aggr sum _ = sum
    count = foldl aggr 0 s

data InterpreterState = InterpreterState
  { code :: String
  , seen :: String
  , input :: String
  , output :: String
  , tape :: Tape
  }

-- |Takes sanitized code and input as string, starting with an empty tape,  
-- and returns both the resulting tape and the output as string 
interpretCode :: String -> String -> (Tape, String)
interpretCode code input = go (InterpreterState code "" input "" emptyTape)
  where
    go :: InterpreterState -> (Tape, String)
    go (InterpreterState "" _ _ out t) = (t, reverse out)
    go s@(InterpreterState (c:code) seen inp out t) =
      case c of
        '[' ->
          if t ^. curr == 0
            -- skip whole loop
            then go s {code = todo, seen = loop ++ ('[' : seen)}
            -- go into loop
            else go s {code = code, seen = '[' : seen}
          where (loop, todo) = partitionByFinding ']' code
        ']' ->
          if t ^. curr == 0
            -- exit loop
            then go s {code = code, seen = ']' : seen}
            -- go back to loop start
            else go s {code = loop ++ (']' : code), seen = remaining}
          where (loop, remaining) = partitionByFinding '[' seen
        '.' -> go s {code = code, seen = '.' : seen, output = readChar t : out}
        ',' ->
          if null inp
            then error "Error: Program requires input but there is none left."
            else go s {code = code, seen = seen', input = inp', tape = tape'}
          where ci:inp' = inp
                tape' = writeChar t ci
                seen' = ',' : seen
        c -> go s {code = code, seen = c : seen, tape = handleChar c t}

{-|
- takes the char to find  and a string to be searched
- ignores occurences enclosed in brackets (even reversed like `]c[`)
- returns (found, remaining) with 'found' in reversed order
- the head of 'found' is always the character to search for
- assumes that there is a solution, throws error if not
- used for finding corresponding braces in 'interpretCode'
-}
partitionByFinding :: Char -> String -> (String, String)
partitionByFinding c toView = go c toView "" 0
  where
    go :: Char -> String -> String -> Int -> (String, String)
    go c [] found _ =
      error $
      "Unexpected error: Failure to find a " ++
      [c] ++ " after finding " ++ found
    go c (h:toView) found 0
      | c == h = (c : found, toView)
    go c (h:toView) found openBrackets =
      case h of
        '[' -> go c toView ('[' : found) (openBrackets + 1)
        ']' -> go c toView (']' : found) (openBrackets - 1)
        other -> go c toView (other : found) openBrackets
