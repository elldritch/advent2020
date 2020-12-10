module Main (main) where

import qualified Advent2020.D1 as D1
import qualified Advent2020.D2 as D2
import qualified Advent2020.D3 as D3
import qualified Advent2020.D4 as D4
import qualified Advent2020.D5 as D5
import qualified Advent2020.D6 as D6
import qualified Advent2020.D7 as D7
import qualified Advent2020.D8 as D8
import Options.Applicative (ParserInfo, auto, briefDesc, execParser, helper, info, long, option, progDesc, strOption)
import Relude

data Options = Options
  { day :: Int,
    part :: Int,
    inputFilepath :: FilePath
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Advent 2020 solutions")
  where
    options =
      Options
        <$> option auto (long "day")
        <*> option auto (long "part")
        <*> strOption (long "input_file")

main :: IO ()
main = do
  Options {day, part, inputFilepath} <- execParser opts
  contents <- readFileText inputFilepath

  case day of
    1 -> case part of
      1 -> runEitherPretty $ D1.run contents D1.part1
      2 -> runEitherPretty $ D1.run contents D1.part2
      _ -> catchAll
    2 -> case part of
      1 -> runEitherPretty $ D2.run contents D2.part1
      2 -> runEitherPretty $ D2.run contents D2.part2
      _ -> catchAll
    3 -> case part of
      1 -> runEitherPretty $ D3.run contents D3.part1
      2 -> runEitherPretty $ D3.run contents D3.part2
      _ -> catchAll
    4 -> case part of
      1 -> runEitherPretty $ D4.run contents D4.part1
      2 -> runEitherPretty $ D4.run contents D4.part2
      _ -> catchAll
    5 -> case part of
      1 -> runEitherPretty $ D5.run contents D5.part1
      2 -> runEitherPretty $ D5.run contents D5.part2
      _ -> catchAll
    6 -> case part of
      1 -> runEitherPretty $ D6.run contents D6.part1
      2 -> runEitherPretty $ D6.run contents D6.part2
      _ -> catchAll
    7 -> case part of
      1 -> runEitherPretty $ D7.run contents D7.part1
      2 -> runEitherPretty $ D7.run contents D7.part2
      _ -> catchAll
    8 -> case part of
      1 -> runEitherPretty $ D8.run contents D8.part1
      2 -> runEitherPretty $ D8.run contents D8.part2
      _ -> catchAll
    _ -> do
      putTextLn "ERROR: no such puzzle"
      exitFailure

runEitherPretty :: (Show t) => Either Text t -> IO ()
runEitherPretty answer = putTextLn $ case answer of
  Right r -> "OK: " <> show r
  Left err -> "ERROR: " <> err

catchAll :: IO ()
catchAll = do
  putTextLn "ERROR: no such part"
  exitFailure
