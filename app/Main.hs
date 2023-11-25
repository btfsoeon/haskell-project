module Main where

import           Config
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           TypingTest
import           UI                        (ui)

argparse :: Parser Arguments
argparse =
  Arguments
    <$> option parseMode (long "mode" <> short 'm' <> value Random)
    <*> option auto (long "time" <> short 't' <> value 30 <> showDefault)
    <*> option auto (long "line_length" <> short 'l' <> value 12 <> showDefault)
    <*> option auto (long "num_words" <> short 'n' <> value 36 <> showDefault)

parseMode :: ReadM Mode
parseMode = do
  string <- readerAsk
  pure
    ( case string of
        "quote"  -> Quote
        "random" -> Random
        _        -> Timed
    )

main :: IO ()
main = do
  conf <- readConfig
  runTest conf =<< execParser opts
    where
      opts = info (argparse <**> helper) (fullDesc <> progDesc "A cli-based typing game written in haskell" <> header "typeracer")


runTest :: Conf -> Arguments ->IO ()
runTest = ui
