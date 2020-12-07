module Advent2020.D4Spec (spec) where

import Advent2020.D4 (Color (..), Length (..), Passport (..), parse)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it)

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
      "iyr:2011 ecl:brn hgt:59in"
    ]

examplePassports :: [Passport]
examplePassports =
  [ Passport
      { birthYear = Just $ Right 1937,
        issueYear = Just $ Right 2017,
        expirationYear = Just $ Right 2020,
        height = Just $ Right $ Centimeters 183,
        hairColor = Just $ Right "fffffd",
        eyeColor = Just $ Right Grey,
        passportID = Just $ Right 860033327,
        countryID = Just "147"
      },
    Passport
      { birthYear = Just $ Right 1929,
        issueYear = Just $ Right 2013,
        expirationYear = Just $ Right 2023,
        height = Nothing,
        hairColor = Just $ Right "cfa07d",
        eyeColor = Just $ Right Amber,
        passportID = Just $ Right 028048884,
        countryID = Just "350"
      },
    Passport
      { birthYear = Just $ Right 1931,
        issueYear = Just $ Right 2013,
        expirationYear = Just $ Right 2024,
        height = Just $ Right $ Centimeters 179,
        hairColor = Just $ Right "ae17e1",
        eyeColor = Just $ Right Brown,
        passportID = Just $ Right 760753108,
        countryID = Nothing
      },
    Passport
      { birthYear = Nothing,
        issueYear = Just $ Right 2011,
        expirationYear = Just $ Right 2025,
        height = Just $ Right $ Inches 59,
        hairColor = Just $ Right "cfa07d",
        eyeColor = Just $ Right Brown,
        passportID = Just $ Right 166559648,
        countryID = Nothing
      }
  ]

exampleInvalidInput :: Text
exampleInvalidInput =
  unlines
    [ "eyr:1972 cid:100",
      "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
      "",
      "iyr:2019",
      "hcl:#602927 eyr:1967 hgt:170cm",
      "ecl:grn pid:012533040 byr:1946",
      "",
      "hcl:dab227 iyr:2012",
      "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
      "",
      "hgt:59cm ecl:zzz",
      "eyr:2038 hcl:74454a iyr:2023",
      "pid:3556412378 byr:2007"
    ]

exampleInvalidPassports :: [Passport]
exampleInvalidPassports =
  [ Passport
      { birthYear = Just (Right 1926),
        issueYear = Just (Right 2018),
        expirationYear = Just (Left "invalid expiration year: 1972"),
        height = Just (Left "1:4:\n  |\n1 | 170\n  |    ^\nunexpected end of input\nexpecting digit or letter\n"),
        hairColor = Just (Right "18171d"),
        eyeColor = Just (Right Amber),
        passportID = Just (Left "1:4:\n  |\n1 | 186cm\n  |    ^\nunexpected 'c'\nexpecting digit\n"),
        countryID = Just "100"
      },
    Passport
      { birthYear = Just (Right 1946),
        issueYear = Just (Right 2019),
        expirationYear = Just (Left "invalid expiration year: 1967"),
        height = Just (Right (Centimeters 170)),
        hairColor = Just (Right "602927"),
        eyeColor = Just (Right Green),
        passportID = Just (Right 12533040),
        countryID = Nothing
      },
    Passport
      { birthYear = Just (Right 1992),
        issueYear = Just (Right 2012),
        expirationYear = Just (Right 2020),
        height = Just (Right (Centimeters 182)),
        hairColor = Just (Left "1:1:\n  |\n1 | dab227\n  | ^\nunexpected 'd'\nexpecting '#'\n"),
        eyeColor = Just (Right Brown),
        passportID = Just (Right 21572410),
        countryID = Just "277"
      },
    Passport
      { birthYear = Just (Left "invalid birth year: 2007"),
        issueYear = Just (Left "invalid issue year: 2023"),
        expirationYear = Just (Left "invalid expiration year: 2038"),
        height = Just (Left "1:5:\n  |\n1 | 59cm\n  |     ^\ninvalid cm: 59\n"),
        hairColor = Just (Left "1:1:\n  |\n1 | 74454a\n  | ^\nunexpected '7'\nexpecting '#'\n"),
        eyeColor = Just (Left "invalid eye color: \"zzz\""),
        passportID = Just (Left "1:10:\n  |\n1 | 3556412378\n  |          ^\nunexpected '8'\nexpecting end of input\n"),
        countryID = Nothing
      }
  ]

exampleValidInput :: Text
exampleValidInput =
  unlines
    [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
      "hcl:#623a2f",
      "",
      "eyr:2029 ecl:blu cid:129 byr:1989",
      "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
      "",
      "hcl:#888785",
      "hgt:164cm byr:2001 iyr:2015 cid:88",
      "pid:545766238 ecl:hzl",
      "eyr:2022",
      "",
      "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    ]

exampleValidPassports :: [Passport]
exampleValidPassports =
  [ Passport
      { birthYear = Just $ Right 1980,
        issueYear = Just $ Right 2012,
        expirationYear = Just $ Right 2030,
        height = Just $ Right $ Inches 74,
        hairColor = Just $ Right "623a2f",
        eyeColor = Just $ Right Green,
        passportID = Just $ Right 087499704,
        countryID = Nothing
      },
    Passport
      { birthYear = Just $ Right 1989,
        issueYear = Just $ Right 2014,
        expirationYear = Just $ Right 2029,
        height = Just $ Right $ Centimeters 165,
        hairColor = Just $ Right "a97842",
        eyeColor = Just $ Right Blue,
        passportID = Just $ Right 896056539,
        countryID = Just "129"
      },
    Passport
      { birthYear = Just $ Right 2001,
        issueYear = Just $ Right 2015,
        expirationYear = Just $ Right 2022,
        height = Just $ Right $ Centimeters 164,
        hairColor = Just $ Right "888785",
        eyeColor = Just $ Right Hazel,
        passportID = Just $ Right 545766238,
        countryID = Just "88"
      },
    Passport
      { birthYear = Just $ Right 1944,
        issueYear = Just $ Right 2010,
        expirationYear = Just $ Right 2021,
        height = Just $ Right $ Centimeters 158,
        hairColor = Just $ Right "b6652a",
        eyeColor = Just $ Right Blue,
        passportID = Just $ Right 093154719,
        countryID = Nothing
      }
  ]

spec :: Spec
spec = do
  it "parses valid passports" $ do
    parse exampleInput `shouldBe'` examplePassports
    parse exampleValidInput `shouldBe'` exampleValidPassports

  it "parses invalid passports" $ do
    parse exampleInvalidInput `shouldBe'` exampleInvalidPassports
