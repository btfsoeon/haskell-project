module TypingTest
  ( Character (..),
    Line,
    Page,
    State (..),
    Player (..),
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
    tick,
    seconds,
    startClock,
    stopClock,
    finalWpm,
    currentWpm,
    isWinner,
    completionPercent,
  )
where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.Char (isSpace)
import Data.List (groupBy, isPrefixOf)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (UTCTime (UTCTime), diffUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import GHC.Read (readField)
import           Lib
import Brick (EventM, Next)
import GHC.IO (unsafePerformIO)
import System.Random (randomRIO)

-- It is often useful to know whether the line / character etc we are
-- considering is "BeforeCursor" or "AfterCursor". More granularity turns out
-- to be unnecessary.
data Position
  = BeforeCursor
  | AfterCursor

data Player = Player
  {
    input :: String,
    finishTime :: Maybe UTCTime
  } deriving Show

data State = State
  { -- target string we want
    target :: String,
    me :: Player,
    cpu :: Player,
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
    gameStartTime :: Maybe UTCTime,
    gameEndTime :: Maybe UTCTime,
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
startClock now s = s {gameStartTime = Just now}

stopClock :: UTCTime -> State -> State
stopClock now s = s {gameEndTime = Just now, me = (me s) {finishTime = Just now}}

hasGameStarted :: State -> Bool
hasGameStarted state =
  startGameTime state <= currentTime state

hasStartedTyping :: State -> Bool
hasStartedTyping = isJust . gameStartTime

hasEnded :: State -> Bool
hasEnded = isJust . gameEndTime

cursorCol :: State -> Int
cursorCol = length . takeWhile (/= '\n') . reverse . input . me

cursorRow :: State -> Int
cursorRow = length . filter (== '\n') . input . me

cursor :: State -> (Int, Int)
cursor s = (cursorCol s, cursorRow s)

atEndOfLine :: State -> Bool
atEndOfLine s = cursorCol s == length (lines (target s) !! cursorRow s)

onLastLine :: State -> Bool
onLastLine s = cursorRow s + 1 == length (lines $ target s)

isComplete :: State -> Bool
isComplete s = input (me s) == target s

isErrorFree :: State -> Bool
isErrorFree s = input (me s) `isPrefixOf` target s

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
      | isSpace c = s {me = (me s) { input = input ( me s) ++ whitespace }}
      | otherwise = s {me = (me s) {input =  input (me s) ++ [c]}}
    whitespace =
      case takeWhile isSpace . drop (length $ input $ me s) $ target s of
        "" -> " "
        ws -> ws

applyBackspace :: State -> State
applyBackspace s = s {me = (cpu s) { input = reverse . drop n . reverse $ input $ me s}}
  where
    n =
      case takeWhile (\(i, t) -> isSpace i && isSpace t) . reverse $
        zip (input $ me s) (target s) of
        [] -> 1
        ws -> length ws

applyBackspaceWord :: State -> State
applyBackspaceWord s = s {me = (cpu s) {input = reverse . drop n . reverse $ input $ me s }}
  where
    n = toWordBeginning . reverse $ input $ me s
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
      me = Player {input = takeWhile isSpace target, finishTime = Nothing},
      cpu = Player {input = takeWhile isSpace target, finishTime = Nothing},
      gameStartTime = Nothing,
      gameEndTime = Nothing,
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
    -- make tuples of lines between what we want (target) and what we have (myInput)
    linePairs = zip (lines $ target s) (lines (input $ me s) ++ repeat "")

noOfChars :: State -> Int
noOfChars = length . input . me

currentWpm :: State -> Player -> IO Int
currentWpm s p = do
  let startTime = gameStartTime s
  if isNothing startTime
    then return 0
    else
      if isJust $ finishTime p
        then do
          let timeElapsed = realToFrac $ diffUTCTime (fromJust $ finishTime p) (fromJust startTime)
              charsPerSecond = fromIntegral (length $ longestCommonPrefix (target s) (input p)) / timeElapsed
          return $ round $ charsPerSecond * 60 / 5
        else do
          timeElapsed <- nowMinusStart s
          let charsPerSecond = fromIntegral (length $ longestCommonPrefix (target s) (input p)) / timeElapsed
          return $ round $ charsPerSecond * 60 / 5

-- the clock has ticked, let's see whether to add cpu input or not
tick :: State -> State
tick s = if hasStartedTyping s
         then let rawInput = input $ cpu s
                  currentString = longestCommonPrefix (target s) (input $ cpu s)
                  inputLength = length rawInput
                  prefixLength = length currentString
                 -- if we are done, no need to type any further
              in if inputLength == length (target s) then
                   if isJust (finishTime $ cpu s) then
                     s
                   else s {cpu = (cpu s) {finishTime = Just now}}
              else case randomNumber of
                 0 -> s
                 1 -> s { cpu = (cpu s) { input = rawInput ++ [nextChar rawInput (target s)]} }
         -- if we haven't started typing don't do anything
         else s
         where now = unsafePerformIO getCurrentTime
               randomNumber = unsafePerformIO randomGen
               randomGen = do
                 randomRIO (0, 1) :: IO Int

nowMinusStart :: State -> IO Double
nowMinusStart s = do
  currentTime <- getCurrentTime
  let startTime = fromJust $ gameStartTime s
  return $ realToFrac $ diffUTCTime currentTime startTime

completionPercent :: State -> Player -> Double
completionPercent s p =
  let prefix = longestCommonPrefix (target s) (input p)
   in fromIntegral (length prefix) / fromIntegral (length (target s))

countChars :: State -> Int
countChars = length . groupBy (\x y -> isSpace x && isSpace y) . target

-- reports whether the given player is the winner
isWinner :: State -> Player -> Bool
isWinner s p = isEarlier (finishTime p) (finishTime (me s)) && isEarlier (finishTime p) (finishTime (cpu s))

seconds :: State -> Double
seconds s = realToFrac $ diffUTCTime (fromJust $ gameEndTime s) (fromJust $ gameStartTime s)

finalWpm :: State -> Int
finalWpm s = round (fromIntegral (countChars s) / (5 * seconds s / 60))

accuracy :: State -> Double
accuracy s = fromIntegral (hits s) / fromIntegral (strokes s)
