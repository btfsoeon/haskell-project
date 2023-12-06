module Lib
    (
    padString
    , bottomDottedBorder
    ) where

import Graphics.Vty (yellow, withForeColor, defAttr)


import Brick (Widget, hBox, str, vBox, padBottom, Padding (..))


padString :: Int -> String -> String
padString len str = take len (str ++ replicate (len - length str) ' ')

bottomDottedBorder :: Int -> Widget a -> Widget a
bottomDottedBorder numChars w =
  let borderChar = '-'
      dottedLine = hBox $ replicate numChars (str [borderChar])
  in vBox [padBottom (Pad 1) w, dottedLine]