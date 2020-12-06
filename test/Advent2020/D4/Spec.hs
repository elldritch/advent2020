module Advent2020.D4.Spec (spec) where

import Advent2020.D4 (Passport (..), parse)
import Relude
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
      "byr:1937 iyr:2017 cid:147 hgt:183cm",
      "",
      "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
      "hcl:#cfa07d byr:1929",
      "",
      "hcl:#ae17e1 iyr:2013",
      "eyr:2024",
      "ecl:brn pid:760753108 byr:1931",
      "hgt:179cm",
      "",
      "hcl:#cfa07d eyr:2025 pid:166559648",
      "iyr:2011 ecl:brn hgt:59in",
      ""
    ]

examplePassports :: [Passport]
examplePassports =
  [ Passport
      { birthYear = Just "1937",
        issueYear = Just "2017",
        expirationYear = Just "2020",
        height = Just "183cm",
        hairColor = Just "#fffffd",
        eyeColor = Just "gry",
        passportID = Just "860033327",
        countryID = Just "147"
      },
    Passport
      { birthYear = Just "1929",
        issueYear = Just "2013",
        expirationYear = Just "2023",
        height = Nothing,
        hairColor = Just "#cfa07d",
        eyeColor = Just "amb",
        passportID = Just "028048884",
        countryID = Just "350"
      },
    Passport
      { birthYear = Just "1931",
        issueYear = Just "2013",
        expirationYear = Just "2024",
        height = Just "179cm",
        hairColor = Just "#ae17e1",
        eyeColor = Just "brn",
        passportID = Just "760753108",
        countryID = Nothing
      },
    Passport
      { birthYear = Nothing,
        issueYear = Just "2011",
        expirationYear = Just "2025",
        height = Just "59in",
        hairColor = Just "#cfa07d",
        eyeColor = Just "brn",
        passportID = Just "166559648",
        countryID = Nothing
      }
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "parses passports" $ do
      let parsed = parse exampleInput
      case parsed of
        Right passports -> passports `shouldBe` examplePassports
        Left err -> expectationFailure $ toString err
