module Main (main) where

import qualified Advent2020.D1 as D1
import Options.Applicative (ParserInfo, argument, briefDesc, command, execParser, helper, info, metavar, progDesc, str, subparser)
import Relude

data PuzzleArgs
  = Day1 FilePath
  deriving (Show)

opts :: ParserInfo PuzzleArgs
opts =
  info
    (cmds <**> helper)
    (briefDesc <> progDesc "Advent 2020 solutions")
  where
    cmds =
      subparser $
        command "1-1" (info (Day1 <$> argument str (metavar "[INPUT_FILEPATH]") <**> helper) (briefDesc <> progDesc "Solve day 1, part 1"))

main :: IO ()
main = do
  args <- execParser opts
  case args of
    Day1 f -> D1.runPart1 f >>= print
