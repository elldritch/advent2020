module Main (main) where

import qualified Advent2020.D1 as D1
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

  print $ case day of
    1 -> case part of
      1 -> show $ D1.run contents D1.part1
      2 -> show $ D1.run contents D1.part2
      _ -> ("No such part" :: Text)
    _ -> ("No such puzzle" :: Text)
