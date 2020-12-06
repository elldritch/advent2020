module Main (main) where

import qualified Advent2020.D1 as D1 (part1, part2, run)
import qualified Advent2020.D2 as D2 (part1, part2, run)
import qualified Advent2020.D3 as D3 (part1, part2, run)
import qualified Advent2020.D4 as D4 ()
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
      1 -> print $ D1.run contents D1.part1
      2 -> print $ D1.run contents D1.part2
      _ -> catchAll
    2 -> case part of
      1 -> print $ D2.run contents D2.part1
      2 -> print $ D2.run contents D2.part2
      _ -> catchAll
    3 -> case part of
      1 -> print $ D3.run contents D3.part1
      2 -> print $ D3.run contents D3.part2
      _ -> catchAll
    4 -> case part of
      1 -> runEitherPretty $ D4.part1 contents
      _ -> catchAll
    _ -> do
      putTextLn "ERROR: no such puzzle"
      exitFailure

runEitherPretty :: (Show t) => Either Text t -> IO ()
runEitherPretty answer = case answer of
  Right r -> print r
  Left err -> putTextLn err

catchAll :: IO ()
catchAll = do
  putTextLn "ERROR: no such part"
  exitFailure
