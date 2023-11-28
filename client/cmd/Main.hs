{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           TypingTest             (initialState)

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

data Config =
  Config
    { fg_empty          :: Word8
    , fg_error          :: Word8
    , files             :: [FilePath]
    , height            :: Int
    , max_paragraph_len :: Int
    , min_paragraph_len :: Int
    , paragraph         :: Bool
    , reflow_           :: Bool
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
    { fg_empty =
        8 &= typ "COLOUR" &=
        help "The ANSI colour code for empty (not yet typed) text"
    , fg_error = 1 &= typ "COLOUR" &= help "The ANSI colour code for errors"
    , height =
        20 &= typ "LINES" &=
        help "The maximum number of lines to sample (default: 20)"
    , max_paragraph_len =
        750 &= typ "WORDS" &=
        help "The maximum length of a sampled paragraph (default: 750)"
    , min_paragraph_len =
        250 &= typ "WORDS" &=
        help "The minimum length of a sampled paragraph (default: 250)"
    , paragraph = def &= help "Sample a paragraph from the input files"
    , reflow_ = def &= help "Reflow paragraph to the target width"
    , tab = 4 &= typ "SIZE" &= help "The size of a tab in spaces (default: 4)"
    , width =
        80 &= typ "CHARS" &=
        help "The width at which to wrap lines (default: 80)"
    , files = def &= args &= typ "FILES"
    } &=
  summary "Typeracer 0.0.0.1" &=
  help "Practice typing against a friend!." &=
  program "gotta-go-fast"

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
      r <- randomRIO (0, max 0 $ length entries - height c)
      let entry = entries !! r in
        return . trimEmptyLines . chop . wrap (width c) . chop . unlines $ lines entry
    ascii = toAscii (tab c) file
    entries = splitOn "^_^" ascii
    chop = unlines . take (height c) . lines

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    c <- cmdArgs config
    file <- readFile "textfiles/passages.txt"
    car <- readFile "textfiles/car.txt"

    target <- sample c file
    let s = initialState target car
    loop <- run (fg_empty c) (fg_error c) s
    when loop main

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 8001 "/" app
