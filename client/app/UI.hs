{-# LANGUAGE OverloadedStrings #-}

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
import           Brick.Widgets.Center   (center, vCenterWith)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime, addUTCTime, secondsToDiffTime, secondsToNominalDiffTime, diffUTCTime, UTCTime, nominalDiffTimeToSeconds)
import           Data.Word              (Word8)
import           Graphics.Vty           (Attr, Color (..), Event (..), Key (..),
                                         Modifier (..), bold, defAttr,
                                         withStyle, mkVty)
import           Text.Printf            (printf)
import           Data.Text              (Text)

import           TypingTest
import           Lib
import Brick.Widgets.Core
import qualified Brick.BChan
import qualified Graphics.Vty.Config as Graphics.Vty
import Control.Concurrent (ThreadId, forkIO, threadDelay, MVar)
import Control.Concurrent.MVar ( newEmptyMVar, takeMVar, putMVar )
import Data.Fixed (div')
import GHC.IO.Unsafe (unsafePerformIO)

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS


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
  printf "%.d words per minute • %.f%% accuracy \nPress Enter to play again • Escape to quit." (finalWpm s) (accuracy s * 100)

drawStartCountdown :: State -> Widget ()
drawStartCountdown s = 
  let gameStartTime = startGameTime s
      originalStartTime = addUTCTime (-(secondsToNominalDiffTime $ fromIntegral $ howMuchOnCounter s)) gameStartTime
      durationTime = diffUTCTime gameStartTime originalStartTime
      elapsedTime = diffUTCTime (currentTime s) originalStartTime

      -- trashy math to get a counter to go from 5 - 1, ugh...
      elapsed = realToFrac (nominalDiffTimeToSeconds elapsedTime) :: Double
      duration = realToFrac (nominalDiffTimeToSeconds durationTime) :: Double
      counterNum = 1 + howMuchOnCounter s - ceiling (fromIntegral (howMuchOnCounter s) * (elapsed / duration))
      in 
        -- Just in case we render too quickly
        if counterNum == 0 || hasGameStarted s then 
          str "GO!"
        else if counterNum < 0 || counterNum > howMuchOnCounter s then 
          str " "
        else 
          str $ show counterNum

-- computeCarPadding will take the percent completion the user is and multiply by the width of
-- the terminal space. So when the user is done, we'll be at 100%
computeCarPadding :: State -> Int -> Int
computeCarPadding s trueWidth = ceiling (completionPercent s * fromIntegral trueWidth)

drawWpm :: State -> Widget ()
drawWpm s = vCenterWith Nothing . str $ padString 8 $ show wpm ++ " wpm"
  where
    wpm = unsafePerformIO $ currentWpm s

drawCar :: State -> Widget ()
drawCar s = let trueWidth = screenWidth s - carWidth s
                leftPad = computeCarPadding s trueWidth
                rightPad = screenWidth s - leftPad - carWidth s in
  vCenterWith Nothing . bottomDottedBorder (screenWidth s) . padRight (Pad rightPad) . padLeft (Pad leftPad) $ str $ car s

draw :: State -> [Widget ()]
draw s
  | hasEnded s = pure . center . padAll 1 . vBox $ [topRow s, drawText s, drawResults s]
  | otherwise =
    pure . center . padAll 1 . vBox $ [topRow s, showCursor () (Location $ cursor s) $ drawText s <=> str " "]
  where topRow s = vBox [drawStartCountdown s, hBox [drawCar s, drawWpm s]]

handleChar :: Char -> State -> EventM () (Next State)
handleChar c s
  | not $ hasGameStarted s = do
    -- ignore character if game hasn't started yet
    continue s
  | not $ hasStartedTyping s = do
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
handleEvent s (AppEvent (Counter i now)) = do
  let nextS = s {counter = counter s + 1, currentTime = now}
  liftIO (WS.sendTextData (conn s) (T.pack (show nextS)))
  --TODO get and update participants' status
  continue nextS
handleEvent s (VtyEvent (EvKey key [MCtrl])) =
  case key of
    -- control C, control D
    KChar 'c' -> halt $ s {loop = False}
    KChar 'd' -> halt $ s {loop = False}
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
      KEsc -> halt $ s {loop = False}
      KEnter   -> halt $ s {loop = True}
      _      -> continue s
  | otherwise =
    case key of
      KChar c -> handleChar c s
      KEnter  -> handleChar '\n' s
      KBS     -> continue $ applyBackspace s
      KEsc    -> halt $ s {loop = False}
      _       -> continue s
handleEvent s _ = continue s

-- handleStartEvent runs when the app first starts up. It records the time  
-- the typing game should start. Note, this is different than when the user
-- starts typing.
handleStartEvent :: State -> EventM () State
handleStartEvent s = do
  now <- liftIO getCurrentTime
  let later = addUTCTime (secondsToNominalDiffTime $ fromIntegral $ howMuchOnCounter s) now
  return s {startGameTime = later}

app :: Attr -> Attr -> Attr -> App State CounterEvent ()
app emptyAttr errorAttr resultAttr =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = handleStartEvent
    , appAttrMap =
        const $
        attrMap
          defAttr
          [ (emptyAttrName, emptyAttr)
          , (errorAttrName, errorAttr)
          , (resultAttrName, resultAttr)
          ]
    }

data CounterEvent = Counter Int UTCTime

counterThread :: Brick.BChan.BChan CounterEvent -> IO ()
counterThread chan = do 
  now <- getCurrentTime
  Brick.BChan.writeBChan chan $ Counter 1 now

setTimer :: MVar Bool -> IO () -> Int -> IO ThreadId
setTimer stop ioOperation ms =
  forkIO $ f
  where
    f = do
      threadDelay (ms*1000)
      shouldStop <- takeMVar stop
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
  setTimer stopFlag (counterThread eventChan) 32
  finalState <- customMain initialVty buildVty
                    (Just eventChan) (app emptyAttr errorAttr resultAttr) initialState
  
  putMVar stopFlag True
  return $ loop finalState
  where
    emptyAttr = fg . ISOColor $ fgEmptyCode
    errorAttr = flip withStyle bold . fg . ISOColor $ fgErrorCode
    -- abusing the fgErrorCode to use as a highlight colour for the results here
    resultAttr = fg . ISOColor $ fgErrorCode