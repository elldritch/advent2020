module Main (main) where

import qualified Advent2020.D1 as D1
import Options.Applicative (CommandFields, Mod, Parser, ParserInfo, argument, briefDesc, command, execParser, helper, hsubparser, info, metavar, progDesc, str)
import Relude

data Day1Puzzles
  = Day1Part1 FilePath
  | Day1Part2 FilePath
  deriving (Show)

newtype PuzzleArgs
  = Day1 Day1Puzzles
  deriving (Show)

opts :: ParserInfo PuzzleArgs
opts =
  info
    (cmds <**> helper)
    (briefDesc <> progDesc "Advent 2020 solutions")
  where
    makeCmd :: String -> Parser t -> String -> Mod CommandFields t
    makeCmd name parser desc = command name (info parser (briefDesc <> progDesc desc))

    -- TODO: makeDay and makePart combinators for arguments.

    cmds =
      hsubparser $
        command
          "day-1"
          (info
            (Day1 <$>
              hsubparser
                  (makeCmd "part-1" (Day1Part1 <$> argument str (metavar "FILE")) "Solve day 1 part 1"
                <> makeCmd "part-2" (Day1Part2 <$> argument str (metavar "FILE")) "Solve day 1 part 2"))
            (briefDesc <> progDesc "Solve day 1"))

main :: IO ()
main = do
  day <- execParser opts
  case day of
    Day1 args -> case args of
      Day1Part1 filepath -> D1.run filepath D1.part1 >>= print
      Day1Part2 filepath -> D1.run filepath D1.part2 >>= print
