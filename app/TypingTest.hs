module TypingTest
  ( Character(..)
  , Line
  , Page
  , State(..)
  , accuracy
  , applyBackspace
  , applyBackspaceWord
  , applyChar
  , atEndOfLine
  , cursor
  , countChars
  , hasEnded
  , hasStarted
  , initialState
  , isComplete
  , onLastLine
  , page
  , seconds
  , startClock
  , stopClock
  , wpm
  ) where

import           Data.Char  (isSpace)
import           Data.List  (groupBy, isPrefixOf)
import           Data.Maybe (fromJust, isJust)
import           Data.Time  (UTCTime, diffUTCTime)

-- It is often useful to know whether the line / character etc we are
-- considering is "BeforeCursor" or "AfterCursor". More granularity turns out
-- to be unnecessary.
data Position
  = BeforeCursor
  | AfterCursor

data State =
  State
    { target  :: String
    , input   :: String
    -- time when the user started typing
    , start   :: Maybe UTCTime
    , end     :: Maybe UTCTime
    , strokes :: Integer
    , hits    :: Integer
    , loop    :: Bool
    }

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

hasStarted :: State -> Bool
hasStarted = isJust . start

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
        hits s' +
        if isErrorFree s'
          then 1
          else 0
    , strokes = strokes s + 1
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
    toWordBeginning (x:y:ys)
      | not (isSpace x) && isSpace y = 1
      | otherwise = 1 + toWordBeginning (y : ys)

initialState :: String -> State
initialState t =
  State
    { target = t
    , input = takeWhile isSpace t
    , start = Nothing
    , end = Nothing
    , strokes = 0
    , hits = 0
    , loop = False
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

-- The following functions are only safe to use when both hasStarted and
-- hasEnded hold.
seconds :: State -> Double
seconds s = realToFrac $ diffUTCTime (fromJust $ end s) (fromJust $ start s)

countChars :: State -> Int
countChars = length . groupBy (\x y -> isSpace x && isSpace y) . target

wpm :: State -> Double
wpm s = fromIntegral (countChars s) / (5 * seconds s / 60)

accuracy :: State -> Double
accuracy s = fromIntegral (hits s) / fromIntegral (strokes s)