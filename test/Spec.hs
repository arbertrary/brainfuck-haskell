import Control.Exception
import Control.Monad (unless, void)
import Data.Char (chr)
import Lib
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.Random (randomRIO)
import Test.HUnit

main :: IO ()
main = do
  results <- runTestTT suite
  if errors results + failures results == 0
    then exitSuccess
    else exitWith $ ExitFailure $ failures results

suite =
  TestList
    [ testBasics
    , testModifyOnlyCurr
    , testHandleChar
    , testCleanupCode
    , testValidateBrackets
    , testPartitionByFinding
    , testInterpretCode
    ]

-- error cases use `print` since it forces evaluation instead of `return`. 
assertThrows :: String -> IO a -> Assertion
assertThrows preface code = do
  errored <- catch (code >> return False) handler
  unless errored $
    assertFailure $ preface ++ ": expected error but there was none."
  where
    handler :: ErrorCall -> IO Bool
    handler err = return True

-- |Fails with the specified msg, indicating a TODO
todoTest msg = TestCase $ assertFailure $ "TODO: " ++ msg

randomTape :: IO Tape
randomTape = do
  curr <- randomRIO (0, 255 :: Int)
  left <- randomList
  right <- randomList
  return $ Tape left curr right

randomList :: IO [Int]
randomList = randomRIO (0, 100 :: Int) >>= go
  where
    go 0 = return []
    go n = do
      r <- randomRIO (0, 255 :: Int)
      rs <- go (n - 1)
      return (r : rs)

testBasics =
  TestLabel "Basic actions" $
  TestList
    [ TestLabel "emptyTape" $
      TestCase $ assertEqual "empty tape" emptyTape $ Tape [] 0 []
    , TestLabel "moveLeft" $
      TestCase $ do
        let expLeft1 = Tape [] 0 [0]
        assertEqual "on empty tape" expLeft1 $ moveLeft emptyTape
        assertEqual "after single <" (Tape [] 0 [0, 0]) $ moveLeft expLeft1
    , TestLabel "moveRight" $
      TestCase $ do
        let expRight1 = Tape [0] 0 []
        assertEqual "on empty tape" expRight1 $ moveRight emptyTape
        assertEqual "after single >" (Tape [0, 0] 0 []) $ moveRight expRight1
    , TestLabel "increment" $
      TestList
        [ TestLabel "regular" $
          TestCase $ do
            assertEqual "on empty tape" (Tape [] 1 []) $ increment emptyTape
            assertEqual "on random tape" (Tape [4, 3, 2] 3 [4, 5]) $
              increment $ Tape [4, 3, 2] 2 [4, 5]
        , TestLabel "overflow" $
          TestCase $ do
            assertEqual "on simple tape" (Tape [] 0 []) $
              increment $ Tape [] 255 []
            assertEqual "on random tape" (Tape [7, 2] 0 [13, 9]) $
              increment $ Tape [7, 2] 255 [13, 9]
        ]
    , TestLabel "decrement" $
      TestList
        [ TestLabel "regular" $
          TestCase $ do
            assertEqual "on simple tape" (Tape [] 13 []) $
              decrement $ Tape [] 14 []
            assertEqual "on random tape" (Tape [1, 2] 5 [6, 3]) $
              decrement $ Tape [1, 2] 6 [6, 3]
        , TestLabel "underflow" $
          TestCase $ do
            assertEqual "on empty tape" (Tape [] 255 []) $ decrement emptyTape
            assertEqual "on random tape" (Tape [3, 9] 255 []) $
              decrement $ Tape [3, 9] 0 []
        ]
    , TestCase $ assertEqual "readChar" '\n' $ readChar $ Tape [1, 4] 10 [17]
    , TestCase $
      assertEqual "writeChar" (Tape [3] 10 [17]) $
      writeChar (Tape [3] 199 [17]) '\n'
    ]

testHandleChar =
  TestLabel "handleChar" $
  TestList
    [ ensureCharCausesFn '>' moveRight
    , ensureCharCausesFn '<' moveLeft
    , ensureCharCausesFn '+' increment
    , ensureCharCausesFn '-' decrement
    , TestLabel "throw on single other" $
      TestCase $ assertThrows "x" $ print $ handleChar 'x' emptyTape
    , TestLabel "throw on other" $
      TestList $
      (\x -> TestCase $ assertThrows (show x) $ print $ handleChar x emptyTape) <$>
      filter (not . (`elem` "<>+-")) (chr <$> [0 .. 255])
    ]
  where
    ensureCharCausesFn :: Char -> (Tape -> Tape) -> Test
    ensureCharCausesFn c fn =
      TestCase $ do
        tape <- randomTape
        assertEqual [c] (fn tape) $ handleChar c tape

ensureModifiesOnlyCurr :: (Tape -> Tape, String) -> Test
ensureModifiesOnlyCurr (fn, msg) =
  TestCase $ do
    tape <- randomTape
    let c = curr tape
    let tape' = fn tape
    assertEqual msg tape $ tape' {curr = c}

testModifyOnlyCurr =
  TestLabel "Basics: modify only current" $
  TestList $
  ensureModifiesOnlyCurr <$>
  [ (increment, "increment")
  , (decrement, "decrement")
  , ((`writeChar` 'X'), "writeChar")
  ]

-- one could test whether this keeps the code in order
-- but that would be out of scope for such a simple function
testCleanupCode =
  TestLabel "cleanupCode" $
  TestCase $
  if all (`elem` ".,<>[]+-") cleaned
    then return ()
    else assertFailure $ "cleanupCode failed with " ++ cleaned
  where
    allChars = chr <$> [0 .. 255]
    cleaned = cleanupCode allChars

testValidateBrackets =
  TestLabel "validateBrackets" $
  TestList
    [ TestLabel "Fine" $
      TestList
        [ TestCase $ assertEqual "0 pairs" Fine $ validateBrackets ",.<>=+"
        , TestCase $ assertEqual "1 pair" Fine $ validateBrackets ",[.>]+-"
        , TestCase $ assertEqual "3 pairs" Fine $ validateBrackets "[,][>].[-,]"
        , TestCase $
          assertEqual "3 nested" Fine $ validateBrackets "[.[<[].,]]+"
        ]
    , TestLabel "NoCode" $
      TestCase $ assertEqual "empty" NoCode $ validateBrackets ""
    , TestLabel "TooManyOpen" $
      TestList
        [ TestCase $
          assertEqual "1 too many" TooManyOpen $ validateBrackets ".[,.+][><"
        , TestCase $
          assertEqual "nested too many" TooManyOpen $
          validateBrackets "[.,[>][<[-]+"
        ]
    , TestLabel "TooManyClosed" $
      TestList
        [ TestCase $
          assertEqual "1 too many" TooManyClosed $ validateBrackets ".[+->]<,]"
        , TestCase $
          assertEqual "nested too many" TooManyClosed $
          validateBrackets "[<[[.+],]+>]]]"
        ]
    ]

testPartitionByFinding =
  TestLabel "partitionByFinding" $
  TestList
    [ TestLabel "should find successfully" $
      TestList $ evalTestCase <$> testCases
    , TestLabel "should fail if not found" $
      TestList $ evalFailCase <$> failCases
    ]
  where
    testCases =
      [ ('x', "[abcx]pxfoo", ("xp]xcba[", "foo"))
      , (']', "[abc[x]yu]foo]bar", ("]oof]uy]x[cba[", "bar"))
      , ('[', "]xx]o[p[][ll[x[", ("[ll[][p[o]xx]", "x["))
      ]
    evalTestCase (c, inp, res) =
      TestCase $ assertEqual inp res $ partitionByFinding c inp
    failCases =
      [('x', "[abcx]]xbca[]x"), (']', "abc[x[d]f[[]g]"), ('[', "][]][]][")]
    evalFailCase (c, inp) =
      TestCase $ assertThrows inp $ print $ partitionByFinding c inp

testInterpretCode =
  TestLabel "interpretCode" $
  TestList
    [ TestCase $
      assertEqual "simple HelloWorld" expected $
      snd $ interpretCode simpleHelloWorld ""
    , TestCase $
      assertEqual "wrapping HelloWorld" expected $
      snd $ interpretCode wrappingHelloWorld ""
    , TestCase $
      assertEqual "negative tape HelloWorld" "Hello, World!" $
      snd $ interpretCode negativeTapeHelloWorld ""
    , TestCase $
      assertEqual "meta-interpreting hello world" expected $
      snd $ interpretCode bfInterpreter simpleHelloWorld
    ]
  where
    expected = "Hello World!\n"

-- a brainf*** self-interpreter
-- http://www.hevanet.com/cristofd/brainfuck/dbfi.b
bfInterpreter =
  ">>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[\
    \->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<\
    \]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>\
    \+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-\
    \[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[\
    \>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]"

-- https://esolangs.org/wiki/Brainfuck
simpleHelloWorld =
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
    \---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.!"

-- https://esolangs.org/wiki/Brainfuck
wrappingHelloWorld =
  ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->\
    \+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+."

-- https://esolangs.org/wiki/Brainfuck
negativeTapeHelloWorld =
  "--<-<<+[+[<+>--->->->-<<<]>]<<--.<++++++.<<-..<<.<+.>>.>>.<<<.+++.>>.>>-.<<<+."
