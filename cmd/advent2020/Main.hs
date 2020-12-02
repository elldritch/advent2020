module Main (main) where

import qualified Advent2020.D1 as D1 (run, part1, part2)
import qualified Advent2020.D2 as D2 (run)
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
      _ -> putTextLn "No such part"
    2 -> case part of
      1 -> print $ D2.run contents
      2 -> undefined
      _ -> putTextLn "No such part"
    _ -> putTextLn "No such puzzle"
