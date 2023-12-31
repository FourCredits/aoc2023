module Day19Spec where

import qualified Data.Map as M
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
    ( M.fromList
        [ ("crn", Workflow {workflowLabel = "crn", conditionalRules = [Conditional {category = X, relation = Gt, value = 2662, goto = "A"}], unconditionalRule = Unconditional {label = "R"}}),
          ("gd", Workflow {workflowLabel = "gd", conditionalRules = [Conditional {category = A, relation = Gt, value = 3333, goto = "R"}], unconditionalRule = Unconditional {label = "R"}}),
          ("hdj", Workflow {workflowLabel = "hdj", conditionalRules = [Conditional {category = M, relation = Gt, value = 838, goto = "A"}], unconditionalRule = Unconditional {label = "pv"}}),
          ("in", Workflow {workflowLabel = "in", conditionalRules = [Conditional {category = S, relation = Lt, value = 1351, goto = "px"}], unconditionalRule = Unconditional {label = "qqz"}}),
          ("lnx", Workflow {workflowLabel = "lnx", conditionalRules = [Conditional {category = M, relation = Gt, value = 1548, goto = "A"}], unconditionalRule = Unconditional {label = "A"}}),
          ("pv", Workflow {workflowLabel = "pv", conditionalRules = [Conditional {category = A, relation = Gt, value = 1716, goto = "R"}], unconditionalRule = Unconditional {label = "A"}}),
          ("px", Workflow {workflowLabel = "px", conditionalRules = [Conditional {category = A, relation = Lt, value = 2006, goto = "qkq"}, Conditional {category = M, relation = Gt, value = 2090, goto = "A"}], unconditionalRule = Unconditional {label = "rfg"}}),
          ("qkq", Workflow {workflowLabel = "qkq", conditionalRules = [Conditional {category = X, relation = Lt, value = 1416, goto = "A"}], unconditionalRule = Unconditional {label = "crn"}}),
          ("qqz", Workflow {workflowLabel = "qqz", conditionalRules = [Conditional {category = S, relation = Gt, value = 2770, goto = "qs"}, Conditional {category = M, relation = Lt, value = 1801, goto = "hdj"}], unconditionalRule = Unconditional {label = "R"}}),
          ("qs", Workflow {workflowLabel = "qs", conditionalRules = [Conditional {category = S, relation = Gt, value = 3448, goto = "A"}], unconditionalRule = Unconditional {label = "lnx"}}),
          ("rfg", Workflow {workflowLabel = "rfg", conditionalRules = [Conditional {category = S, relation = Lt, value = 537, goto = "gd"}, Conditional {category = X, relation = Gt, value = 2440, goto = "R"}], unconditionalRule = Unconditional {label = "A"}})
        ]
    )
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
