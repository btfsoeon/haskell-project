module Lib
    (
    padString
    , bottomDottedBorder
    , longestCommonPrefix
    , nextChar
    , isEarlier
    ) where

import Graphics.Vty (yellow, withForeColor, defAttr)


import Brick (Widget, hBox, str, vBox, padBottom, Padding (..))
import Data.Time (UTCTime)


padString :: Int -> String -> String
padString len str = take len (str ++ replicate (len - length str) ' ')

bottomDottedBorder :: Int -> Widget a -> Widget a
bottomDottedBorder numChars w =
  let borderChar = '-'
      dottedLine = hBox $ replicate numChars (str [borderChar])
  in vBox [padBottom (Pad 1) w, dottedLine]

longestCommonPrefix :: String -> String -> String
longestCommonPrefix (x : xs) (y : ys) | x == y = x : longestCommonPrefix xs ys
longestCommonPrefix _ _ = ""

nextChar :: String -> String -> Char
nextChar a b = case (a, b) of
         ([], []) -> error "can never happen"
         (_, []) -> error "can never happen"
         ([], _) -> head b
         (ah:at, bh:bt) -> if ah == bh
                           then nextChar at bt
                           else error "a should be a prefix of b"

-- returns whether a time is earlier or not - if they are equal then give benefit of the doubt
isEarlier :: Maybe UTCTime -> Maybe UTCTime -> Bool
isEarlier Nothing _ = False
isEarlier _ Nothing = True
isEarlier x y = x <= y
