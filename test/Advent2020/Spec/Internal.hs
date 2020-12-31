module Advent2020.Spec.Internal
  ( shouldBe',
    fromRight',
  )
where

import Relude
import Test.Hspec (Expectation, expectationFailure, shouldBe)

-- | Like 'Test.Hspec.shouldBe', but with pretty-printing for @'Either' 'Text'@
-- errors.
shouldBe' :: (HasCallStack, Show t, Eq t) => Either Text t -> t -> Expectation
shouldBe' actual expected = case actual of
  Right r -> r `shouldBe` expected
  Left err -> expectationFailure $ toString err

fromRight' :: (MonadFail m) => Either Text t -> m t
fromRight' e = case e of
  Right r -> return r
  Left l -> fail $ toString $ "fromRight': " <> l
