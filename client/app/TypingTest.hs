module TypingTest
  ( Character (..),
    Line,
    Page,
    State (..),
    accuracy,
    applyBackspace,
    applyBackspaceWord,
    applyChar,
    atEndOfLine,
    cursor,
    cursorCol,
    cursorRow,
    countChars,
    hasEnded,
    hasGameStarted,
    hasStartedTyping,
    initialState,
    isComplete,
    onLastLine,
    page,
    seconds,
    startClock,
    stopClock,
    finalWpm,
    currentWpm,
    completionPercent,
  )
where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.Char (isSpace)
import Data.List (groupBy, isPrefixOf)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (UTCTime (UTCTime), diffUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import GHC.Read (readField)

-- It is often useful to know whether the line / character etc we are
-- considering is "BeforeCursor" or "AfterCursor". More granularity turns out
-- to be unnecessary.
data Position
  = BeforeCursor
  | AfterCursor

data State = State
  { -- target string we want
    target :: String,
    -- input string user has put in
    input :: String,
    -- car string
    car :: String,
    -- screen width
    screenWidth :: Int,
    -- screen width
    carWidth :: Int,
    -- count down timer
    counter :: Int,
    -- how much we should count down by before starting
    howMuchOnCounter :: Int,
    -- time when the game should start
    startGameTime :: UTCTime,
    -- current time
    currentTime :: UTCTime,
    -- time when the user started typing
    start :: Maybe UTCTime,
    end :: Maybe UTCTime,
    strokes :: Integer,
    hits :: Integer,
    loop :: Bool
  } deriving (Show)

-- For ease of rendering a character in the UI, we tag it as a Hit, Miss, or
-- Empty. Corresponding to the cases of being correctly typed, incorrectly
-- typed (or skipped), or not yet typed.
data Character
  = Hit Char
  | Miss Char
  | Empty Char

type Line = [Character]

type Page = [Line]

startClock :: UTCTime -> State -> State
startClock now s = s {start = Just now}

stopClock :: UTCTime -> State -> State
stopClock now s = s {end = Just now}

hasGameStarted :: State -> Bool
hasGameStarted state =
  startGameTime state <= currentTime state

hasStartedTyping :: State -> Bool
hasStartedTyping = isJust . start

hasEnded :: State -> Bool
hasEnded = isJust . end

cursorCol :: State -> Int
cursorCol = length . takeWhile (/= '\n') . reverse . input

cursorRow :: State -> Int
cursorRow = length . filter (== '\n') . input

cursor :: State -> (Int, Int)
cursor s = (cursorCol s, cursorRow s)

atEndOfLine :: State -> Bool
atEndOfLine s = cursorCol s == length (lines (target s) !! cursorRow s)

onLastLine :: State -> Bool
onLastLine s = cursorRow s + 1 == length (lines $ target s)

isComplete :: State -> Bool
isComplete s = input s == target s

isErrorFree :: State -> Bool
isErrorFree s = input s `isPrefixOf` target s

applyChar :: Char -> State -> State
applyChar c s =
  s'
    { hits =
        hits s'
          + if isErrorFree s'
            then 1
            else 0,
      strokes = strokes s + 1
    }
  where
    s'
      | isSpace c = s {input = input s ++ whitespace}
      | otherwise = s {input = input s ++ [c]}
    whitespace =
      case takeWhile isSpace . drop (length $ input s) $ target s of
        "" -> " "
        ws -> ws

applyBackspace :: State -> State
applyBackspace s = s {input = reverse . drop n . reverse $ input s}
  where
    n =
      case takeWhile (\(i, t) -> isSpace i && isSpace t) . reverse $
        zip (input s) (target s) of
        [] -> 1
        ws -> length ws

applyBackspaceWord :: State -> State
applyBackspaceWord s = s {input = reverse . drop n . reverse $ input s}
  where
    n = toWordBeginning . reverse $ input s
    toWordBeginning "" = 0
    toWordBeginning [c] = 1
    toWordBeginning (x : y : ys)
      | not (isSpace x) && isSpace y = 1
      | otherwise = 1 + toWordBeginning (y : ys)

initialState :: String -> String -> State
initialState target car =
  State
    { target = target,
      car = car,
      counter = 0,
      -- Use 80 as the default min screen width
      screenWidth = maximum (map length (lines target) ++ [80]),
      carWidth = maximum (map length (lines car)),
      input = takeWhile isSpace target,
      start = Nothing,
      end = Nothing,
      howMuchOnCounter = 5,
      startGameTime = UTCTime (fromGregorian 1970 0 1) (secondsToDiffTime 0),
      currentTime = UTCTime (fromGregorian 1970 0 1) (secondsToDiffTime 0),
      strokes = 0,
      hits = 0,
      loop = True
    }

character :: Position -> (Maybe Char, Maybe Char) -> Character
character _ (Just t, Just i)
  | t == i = Hit t
  | t /= i = Miss i
character _ (Nothing, Just i) = Miss i
character BeforeCursor (Just t, Nothing) = Miss t
character AfterCursor (Just t, Nothing) = Empty t

line :: Position -> (String, String) -> Line
line _ ("", "") = []
line p (ts, is) = map (character p) charPairs
  where
    charPairs = take maxLen $ zip (nothingsForever ts) (nothingsForever is)
    nothingsForever x = map Just x ++ repeat Nothing
    maxLen = max (length ts) (length is)

page :: State -> Page
page s = linesBeforeCursor ++ linesAfterCursor
  where
    linesBeforeCursor = map (line BeforeCursor) $ take (cursorRow s) linePairs
    linesAfterCursor = map (line AfterCursor) $ drop (cursorRow s) linePairs
    -- make tuples of lines between what we want (target) and what we have (input)
    linePairs = zip (lines $ target s) (lines (input s) ++ repeat "")

noOfChars :: State -> Int
noOfChars = length . input

currentWpm :: State -> IO Int
currentWpm s = do
  let startTime = start s
  let endTime = end s
  if isNothing startTime
    then return 0
    else
      if isJust endTime
        then return $ finalWpm s
        else do
          timeElapsed <- nowMinusStart s
          let charsPerSecond = fromIntegral (length $ longestCommonPrefix (target s) (input s)) / timeElapsed
          return $ round $ charsPerSecond * 60 / 5

nowMinusStart :: State -> IO Double
nowMinusStart s = do
  currentTime <- getCurrentTime
  let startTime = fromJust $ start s
  return $ realToFrac $ diffUTCTime currentTime startTime

longestCommonPrefix :: String -> String -> String
longestCommonPrefix (x : xs) (y : ys) | x == y = x : longestCommonPrefix xs ys
longestCommonPrefix _ _ = ""

completionPercent :: State -> Double
completionPercent s =
  let prefix = longestCommonPrefix (target s) (input s)
   in fromIntegral (length prefix) / fromIntegral (length (target s))

countChars :: State -> Int
countChars = length . groupBy (\x y -> isSpace x && isSpace y) . target

-- The following functions are only safe to use when both hasStarted and
-- hasEnded hold.
seconds :: State -> Double
seconds s = realToFrac $ diffUTCTime (fromJust $ end s) (fromJust $ start s)

finalWpm :: State -> Int
finalWpm s = round (fromIntegral (countChars s) / (5 * seconds s / 60))

accuracy :: State -> Double
accuracy s = fromIntegral (hits s) / fromIntegral (strokes s)
