module Advent2020.Spec.Internal (shouldBe') where

import Relude
import Test.Hspec (Expectation, expectationFailure, shouldBe)

-- | Like 'Test.Hspec.shouldBe', but with pretty-printing for @'Either' 'Text'@
-- errors.
shouldBe' :: (Show t, Eq t) => Either Text t -> t -> Expectation
shouldBe' actual expected = case actual of
  Right r -> r `shouldBe` expected
  Left err -> expectationFailure $ toString err
