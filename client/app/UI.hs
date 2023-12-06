module UI
  ( run
  ) where

import           Brick                  (App (..), AttrName, BrickEvent (..),
                                         EventM, Location (..), Next,
                                         Padding (..), Widget, attrMap,
                                         attrName, continue, defaultMain,
                                         emptyWidget, fg, halt, padAll,
                                         padBottom, showCursor, showFirstCursor,
                                         str, withAttr, (<+>), (<=>), vBox, padTop)
import           Brick.Widgets.Center   (center, vCenterWith)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime)
import           Data.Word              (Word8)
import           Graphics.Vty           (Attr, Color (..), Event (..), Key (..),
                                         Modifier (..), bold, defAttr,
                                         withStyle)
import           Text.Printf            (printf)

import           TypingTest
import           Lib
import Brick.Widgets.Core
import GHC.IO.Unsafe (unsafePerformIO)

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
  printf "%.f words per minute • %.f%% accuracy" (finalWpm s) (accuracy s * 100)

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
  where topRow s = hBox [drawCar s, drawWpm s]


handleChar :: Char -> State -> EventM () (Next State)
handleChar c s
  | not $ hasStarted s = do
    now <- liftIO getCurrentTime
    continue $ startClock now s'
  | isComplete s' = do
    now <- liftIO getCurrentTime
    continue $ stopClock now s'
  | otherwise = continue s'
  where
    s' = applyChar c s

handleEvent :: State -> BrickEvent () e -> EventM () (Next State)
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

app :: Attr -> Attr -> Attr -> App State e ()
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

run :: Word8 -> Word8 -> State -> IO Bool
run fgEmptyCode fgErrorCode initialState = do
  s <- defaultMain (app emptyAttr errorAttr resultAttr) initialState
  return $ loop s
  where
    emptyAttr = fg . ISOColor $ fgEmptyCode
    errorAttr = flip withStyle bold . fg . ISOColor $ fgErrorCode
    -- abusing the fgErrorCode to use as a highlight colour for the results here
    resultAttr = fg . ISOColor $ fgErrorCode