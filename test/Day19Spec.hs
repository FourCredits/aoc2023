module Day19Spec where

import Day19
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "parsing" $ do
    it "should parse correctly" $ do
      parse exampleInput `shouldBe` Right exampleParsed

  context "example" $ do
    it "part 1" $ part1 exampleParsed `shouldBe` 19114
    it "part 2" $ part2 exampleParsed `shouldBe` 167409079868000

  context "real" $ do
    real <- realInput 19 & runIO
    it "part 1" $ part1 <$> parse real `shouldBe` Right 325952
    it "part 2" $ part2 <$> parse real `shouldBe` Right 125744206494820

exampleParsed :: Input
exampleParsed =
  Input
    [ Workflow
        { name = "px",
          conditionalRules =
            [ Conditional A Lt 2006 "qkq",
              Conditional M Gt 2090 "A"
            ],
          unconditionalRule = Unconditional "rfg"
        },
      Workflow
        { name = "pv",
          conditionalRules = [Conditional A Gt 1716 "R"],
          unconditionalRule = Unconditional "A"
        },
      Workflow
        { name = "lnx",
          conditionalRules = [Conditional M Gt 1548 "A"],
          unconditionalRule = Unconditional "A"
        },
      Workflow
        { name = "rfg",
          conditionalRules = [Conditional S Lt 537 "gd", Conditional X Gt 2440 "R"],
          unconditionalRule = Unconditional "A"
        },
      Workflow
        { name = "qs",
          conditionalRules = [Conditional S Gt 3448 "A"],
          unconditionalRule = Unconditional "lnx"
        },
      Workflow
        { name = "qkq",
          conditionalRules = [Conditional X Lt 1416 "A"],
          unconditionalRule = Unconditional "crn"
        },
      Workflow
        { name = "crn",
          conditionalRules = [Conditional X Gt 2662 "A"],
          unconditionalRule = Unconditional "R"
        },
      Workflow
        { name = "in",
          conditionalRules = [Conditional S Lt 1351 "px"],
          unconditionalRule = Unconditional "qqz"
        },
      Workflow
        { name = "qqz",
          conditionalRules = [Conditional S Gt 2770 "qs", Conditional M Lt 1801 "hdj"],
          unconditionalRule = Unconditional "R"
        },
      Workflow
        { name = "gd",
          conditionalRules = [Conditional A Gt 3333 "R"],
          unconditionalRule = Unconditional "R"
        },
      Workflow
        { name = "hdj",
          conditionalRules = [Conditional M Gt 838 "A"],
          unconditionalRule = Unconditional "pv"
        }
    ]
    [ Part {x = 787, m = 2655, a = 1222, s = 2876},
      Part {x = 1679, m = 44, a = 2067, s = 496},
      Part {x = 2036, m = 264, a = 79, s = 2244},
      Part {x = 2461, m = 1339, a = 466, s = 291},
      Part {x = 2127, m = 1623, a = 2188, s = 1013}
    ]

exampleInput :: Text
exampleInput =
  "px{a<2006:qkq,m>2090:A,rfg}\n\
  \pv{a>1716:R,A}\n\
  \lnx{m>1548:A,A}\n\
  \rfg{s<537:gd,x>2440:R,A}\n\
  \qs{s>3448:A,lnx}\n\
  \qkq{x<1416:A,crn}\n\
  \crn{x>2662:A,R}\n\
  \in{s<1351:px,qqz}\n\
  \qqz{s>2770:qs,m<1801:hdj,R}\n\
  \gd{a>3333:R,R}\n\
  \hdj{m>838:A,pv}\n\
  \\n\
  \{x=787,m=2655,a=1222,s=2876}\n\
  \{x=1679,m=44,a=2067,s=496}\n\
  \{x=2036,m=264,a=79,s=2244}\n\
  \{x=2461,m=1339,a=466,s=291}\n\
  \{x=2127,m=1623,a=2188,s=1013}"
