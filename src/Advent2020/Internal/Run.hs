module Advent2020.Internal.Run
  ( simpleRun,
    simpleRun',
    runNumbers,
    runNumbers',
  )
where

import Advent2020.Internal.Parse (parseNumbers)
import Relude

-- | Runner that binds directly to runner.
simpleRun :: (Text -> Either Text input) -> (input -> Either Text output) -> Text -> Either Text output
simpleRun parser runner contents = parser contents >>= runner

-- | Like 'simpleRun', but specifies a function to transform the output of a runner.
simpleRun' :: (Text -> Either Text input) -> (runnerOutput -> Either Text output) -> (input -> runnerOutput) -> Text -> Either Text output
simpleRun' parser runTo runner contents = parser contents >>= runTo . runner

-- | Runner for questions that take number list inputs.
runNumbers :: (NonEmpty Int -> Either Text Int) -> Text -> Either Text Int
runNumbers runner contents = do
  xs <- parseNumbers contents
  runner xs

-- | Like 'runNumbers', but for '[@Int@]' rather than '@NonEmpty@ @Int@'.
runNumbers' :: ([Int] -> Either Text Int) -> Text -> Either Text Int
runNumbers' r = runNumbers (r . toList)
