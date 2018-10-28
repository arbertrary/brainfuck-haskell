module Main where

import Control.Monad (when)
import Data.List (intercalate)
import Lib
import System.Environment (getArgs)

-- TODO: let input have empty lines without failing
main :: IO ()
main = do
  args <- getArgs
  let isVerbose =
        not (null args) && head args `elem` ["-v", "--verbose"]
  putStrLn "\n>> CODE >>\n"
  codeLines <- takeLinesUntil null
  let inputIsIncluded = any ('!' `elem`) codeLines
  let singleLineCode = intercalate "" codeLines
  let code = cleanupCode singleLineCode
  when (isVerbose && code /= singleLineCode) $ do
    putStrLn ">> CLEANED CODE >>\n"
    putStrLn code
    putStrLn ""
  case validateBrackets code of
    TooManyOpen -> putStrLn "Invalid code: Too many open brackets. Exiting..."
    TooManyClosed ->
      putStrLn "Invalid code: Too many closing brackets. Exiting..."
    NoCode -> putStrLn "No code entered. Exiting..."
    Fine -> do
      input <-
        if inputIsIncluded
          then do
            let codeInLines = intercalate "\n" codeLines
            return $ tail $ dropWhile (/= '!') codeInLines
          else do
            putStrLn ">> INPUT >>\n"
            inputLines <- takeLinesUntil null
            return $ unlines inputLines
      let (tape, out) = interpretCode code input
      putStrLn ">> OUTPUT >>\n"
      putStrLn out
      when isVerbose $ do
        putStrLn "\n>> TAPE >>\n"
        putStrLn $ tail $ init $ show tape -- cut weird "s

takeLinesUntil :: (String -> Bool) -> IO [String]
takeLinesUntil pred = go pred []
  where
    go pred lines = do
      line <- getLine
      if pred line
        then return $ reverse lines
        else go pred $ line : lines
