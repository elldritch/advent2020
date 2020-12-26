module Advent2020.Internal.Debug
  ( traceWith,
    tracePrefix,
    label,
  )
where

import Data.Either.Extra (mapLeft)
import Relude

-- | 'trace' a message derived from the traced value.
{-# WARNING traceWith "'traceWith' remains in code" #-}
traceWith :: (a -> Text) -> a -> a
traceWith f x = trace (toString $ f x) x

-- | 'trace' a message derived from the traced value.
{-# WARNING tracePrefix "'tracePrefix' remains in code" #-}
tracePrefix :: (Show a) => Text -> a -> a
tracePrefix msg x = trace (toString $ msg <> ": " <> show x) x

-- | Like Megaparsec's 'Text.Megaparsec.label', but for nice error messages with
-- @'Either' 'Text'@.
label :: Text -> Either Text t -> Either Text t
label name = mapLeft (\err -> "error while " <> name <> ": " <> err)
