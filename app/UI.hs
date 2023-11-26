module UI
  ( run
  ) where

import           Brick                  (App (..), AttrName, BrickEvent (..),
                                         EventM, Location (..), Next,
                                         Padding (..), Widget, attrMap,
                                         attrName, continue, defaultMain,
                                         emptyWidget, fg, halt, padAll,
                                         padBottom, showCursor, showFirstCursor,
                                         str, withAttr, (<+>), (<=>), vBox, padTop, customMain)
import           Brick.Widgets.Center   (center)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime)
import           Data.Word              (Word8)
import           Graphics.Vty           (Attr, Color (..), Event (..), Key (..),
                                         Modifier (..), bold, defAttr,
                                         withStyle, mkVty)
import           Text.Printf            (printf)

import           TypingTest
import Brick.Widgets.Core
import qualified Brick.BChan
import qualified Graphics.Vty.Config as Graphics.Vty
import Control.Concurrent (ThreadId, forkIO, threadDelay, MVar)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Concurrent.MVar (putMVar)

emptyAttrName :: AttrName
emptyAttrName = attrName "empty"

errorAttrName :: AttrName
errorAttrName = attrName "error"

resultAttrName :: AttrName
resultAttrName = attrName "result"

drawCharacter :: Character -> Widget ()
drawCharacter (Hit c)    = str [c]
drawCharacter (Miss ' ') = withAttr errorAttrName $ str ['_']
drawCharacter (Miss c)   = withAttr errorAttrName $ str [c]
drawCharacter (Empty c)  = withAttr emptyAttrName $ str [c]

drawLine :: Line -> Widget ()
-- We display an empty line as a single space so that it still occupies
-- vertical space.
drawLine [] = str " "
drawLine ls = foldl1 (<+>) $ map drawCharacter ls

drawText :: State -> Widget ()
drawText s = padBottom (Pad 2) . foldl (<=>) emptyWidget . map drawLine $ page s

drawResults :: State -> Widget ()
drawResults s =
  withAttr resultAttrName . str $
  printf "%.f words per minute • %.f%% accuracy" (wpm s) (accuracy s * 100)

longestCommonPrefix :: String -> String -> String
longestCommonPrefix (x:xs) (y:ys) | x == y = x:longestCommonPrefix xs ys
longestCommonPrefix _ _ = ""

-- computeCarPadding will take the percent completion the user is and multiply by the width of
-- the terminal space. So when the user is done, we'll be at 100%
computeCarPadding :: State -> Int
computeCarPadding s = 
  let prefix = longestCommonPrefix (target s) (input s)
      completionPercent = fromIntegral (length prefix) / fromIntegral (length (target s))
      -- true width is the width of our screen minus the length of the car
      trueWidth = screenWidth s - carWidth s
      in ceiling (completionPercent * fromIntegral trueWidth)

drawCar :: State -> Widget ()
drawCar s = padLeft (Pad $ computeCarPadding s) . padTop (Pad 1) . padBottom (Pad 1) $ str $ car s 

drawCounter :: State -> Widget ()
drawCounter s = str $ show $ counter s
  
draw :: State -> [Widget ()]
draw s
  | hasEnded s = pure . center . padAll 1 . vBox $ [drawCar s, drawText s, drawResults s]
  | otherwise =
    pure . center . padAll 1 . vBox $ [drawCounter s, drawCar s, showCursor () (Location $ cursor s) $ drawText s <=> str " "]


handleChar :: Char -> State -> EventM () (Next State)
handleChar c s
  | not $ hasStarted s = do
    now <- liftIO getCurrentTime
    continue $ startClock now s'
  | isComplete s' = do
    now <- liftIO getCurrentTime
    continue $ stopClock now s'
  | otherwise = 
    continue s'
  where
    s' = applyChar c s

handleEvent :: State -> BrickEvent () CounterEvent -> EventM () (Next State)
handleEvent s (AppEvent (Counter i)) = continue s {counter = counter s + 1}
handleEvent s (VtyEvent (EvKey key [MCtrl])) =
  case key of
    -- control C, control D
    KChar 'c' -> halt s
    KChar 'd' -> halt s
    KChar 'w' -> continue $ applyBackspaceWord s
    KBS       -> continue $ applyBackspaceWord s
    _         -> continue s
handleEvent s (VtyEvent (EvKey key [MAlt])) =
  case key of
    KBS -> continue $ applyBackspaceWord s
    _   -> continue s
handleEvent s (VtyEvent (EvKey key [MMeta])) =
  case key of
    KBS -> continue $ applyBackspaceWord s
    _   -> continue s
handleEvent s (VtyEvent (EvKey key []))
  | hasEnded s =
    case key of
      KEnter -> halt s
      KEsc   -> halt $ s {loop = True}
      _      -> continue s
  | otherwise =
    case key of
      KChar c -> handleChar c s
      KEnter  -> handleChar '\n' s
      KBS     -> continue $ applyBackspace s
      KEsc    -> halt $ s {loop = True}
      _       -> continue s
handleEvent s _ = continue s

app :: Attr -> Attr -> Attr -> App State CounterEvent ()
app emptyAttr errorAttr resultAttr =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap =
        const $
        attrMap
          defAttr
          [ (emptyAttrName, emptyAttr)
          , (errorAttrName, errorAttr)
          , (resultAttrName, resultAttr)
          ]
    }

data CounterEvent = Counter Int

counterThread :: Brick.BChan.BChan CounterEvent -> IO ()
counterThread chan = do
    putStrLn "Hello, world!" -- Print the message
    Brick.BChan.writeBChan chan $ Counter 1
    putStrLn "goodbye" -- Print the message

setTimer :: MVar Bool -> IO () -> Int -> IO ThreadId
setTimer stop ioOperation ms =
  forkIO $ do
    f
  where 
    f = do
      putStrLn "here"
      threadDelay (ms*1000)
      shouldStop <- takeMVar stop
      putStrLn "there"
      if shouldStop then
        return ()
      else do
          threadDelay (ms*1000)
          ioOperation
          putMVar stop False
          f

run :: Word8 -> Word8 -> State -> IO Bool
run fgEmptyCode fgErrorCode initialState = do
  stopFlag <- newEmptyMVar
  putMVar stopFlag False

  eventChan <- Brick.BChan.newBChan 10
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  -- set a timer to keep running
  setTimer stopFlag (counterThread eventChan) 1000
  finalState <- customMain initialVty buildVty
                    (Just eventChan) (app emptyAttr errorAttr resultAttr) initialState
  
  putMVar stopFlag True
  return $ loop finalState
  where
    emptyAttr = fg . ISOColor $ fgEmptyCode
    errorAttr = flip withStyle bold . fg . ISOColor $ fgErrorCode
    -- abusing the fgErrorCode to use as a highlight colour for the results here
    resultAttr = fg . ISOColor $ fgErrorCode