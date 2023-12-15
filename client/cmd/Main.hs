{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where

import           Control.Monad          (filterM, when)
import           Data.Char              (isAscii, isPrint)
import           Data.FileEmbed         (embedStringFile)
import           Data.List.Split        (splitOn)
import qualified Data.Text              as T
import           Data.Word              (Word8)
import           System.Console.CmdArgs (Data, Typeable, args, cmdArgs, def,
                                         details, help, name, program, summary,
                                         typ, (&=))
import           System.Directory       (doesFileExist)
import           System.Random          (randomRIO)
import           Text.Wrap              (WrapSettings (..), defaultWrapSettings,
                                         wrapText)

import           UI                     (run)
import           Typeracer             (initialState)

data Config =
  Config
    { fg_hit            :: Word8
    , fg_empty          :: Word8
    , fg_error          :: Word8
    , files             :: [FilePath]
    , tab               :: Int
    , width             :: Int
    }
  deriving (Show, Data, Typeable)

toAscii :: Int -> String -> String
toAscii tabWidth = concatMap toAscii'
  where
    toAscii' c
      | c == '\t' = replicate tabWidth ' '
      | c == '‘' || c == '’' = "'"
      | c == '“' || c == '”' = "\""
      | c == '–' || c == '—' = "-"
      | c == '…' = "..."
      | isAscii c && (isPrint c || c == '\n') = [c]
      | otherwise = ""

trimEmptyLines :: String -> String
trimEmptyLines = f . f
  where
    f = reverse . dropWhile (== '\n')

config :: Config
config =
  Config
    { 
      fg_hit =
        6 &= typ "COLOUR" &=
        help "The ANSI colour code for typed text"
    , fg_empty =
        0 &= typ "COLOUR" &=
        help "The ANSI colour code for empty (not yet typed) text"
    , fg_error = 1 &= typ "COLOUR" &= help "The ANSI colour code for errors"
    , tab = 4 &= typ "SIZE" &= help "The size of a tab in spaces (default: 4)"
    , width =
        80 &= typ "CHARS" &=
        help "The width at which to wrap lines (default: 80)"
    , files = def &= args &= typ "FILES"
    } &=
  summary "Typeracer 0.1.0.0" &=
  help "Practice typing against a cpu!." &=
  program "typeracer"

wrap :: Int -> String -> String
wrap width = T.unpack . wrapText wrapSettings width . T.pack

wrapSettings = defaultWrapSettings {preserveIndentation = True, breakLongWords = True}

loopWhile :: Monad m => (a -> Bool) -> m a -> m a
loopWhile p mx = do
  x <- mx
  if p x
    then loopWhile p mx
    else return x

sample :: Config -> String -> IO String
sample c file = sampleLine
  where
    sampleLine = do
      r <- randomRIO (0, max 0 $ length entries - 1)
      let entry = entries !! r in
        return . trimEmptyLines . wrap (width c) . unlines $ lines entry
    ascii = toAscii (tab c) file
    entries = splitOn "^_^" ascii

main :: IO ()
main = do
  c <- cmdArgs config
  file <- readFile "textfiles/passages.txt"
  car <- readFile "textfiles/car.txt"

  target <- sample c file
  let s = initialState target car
  loop <- run (fg_hit c) (fg_empty c) (fg_error c) s
  when loop main
