{-# LANGUAGE RankNTypes #-}

module Advent2020.D4 (run, part1, part2) where

import Relude
import Advent2020.Internal.D4 (Passport(..), parse)

run :: Text -> (Passport -> Bool) -> Either Text Int
run contents runner = do
  ps <- parse contents
  return $ length $ filter runner ps

checkPassport :: (forall a. Maybe (Either Text a) -> Bool) -> Passport -> Bool
checkPassport f Passport {..} =
  f birthYear
    && f issueYear
    && f expirationYear
    && f height
    && f hairColor
    && f eyeColor
    && f passportID

part1 :: Passport -> Bool
part1 = checkPassport isJust

part2 :: Passport -> Bool
part2 = checkPassport isOK
  where
    isOK (Just (Right _)) = True
    isOK _ = False
