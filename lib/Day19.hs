{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day19
  ( parse,
    Workflow (..),
    Part (..),
    ConditionalRule (..),
    UnconditionalRule (..),
    Category (..),
    Relation (..),
    Input (..),
    part1,
    part2,
    solve,
  )
where

import qualified Data.Map as M
import qualified Text.Parsec as P

data Input = Input
  { workflows :: Map Label Workflow,
    parts :: [Part]
  }
  deriving (Show, Eq)

type Label = Text

data Workflow = Workflow
  { workflowLabel :: Label,
    conditionalRules :: [ConditionalRule],
    unconditionalRule :: UnconditionalRule
  }
  deriving (Show, Eq)

data ConditionalRule = Conditional
  { category :: Category,
    relation :: Relation,
    value :: Int,
    goto :: Label
  }
  deriving (Show, Eq)

newtype UnconditionalRule = Unconditional {label :: Label} deriving (Show, Eq)

data Category = X | M | A | S deriving (Show, Eq)

data Relation = Lt | Gt deriving (Show, Eq)

type Rating = Int

data Part = Part {x :: Rating, m :: Rating, a :: Rating, s :: Rating}
  deriving (Show, Eq)

data Range = Range
  { rangeLabel :: Label,
    xRange :: (Rating, Rating),
    mRange :: (Rating, Rating),
    aRange :: (Rating, Rating),
    sRange :: (Rating, Rating)
  }

solve :: Text -> (Text, Text)
solve = (display part1 &&& display part2) . parse
  where
    display f = either (const "parse errror") (show . f)

parse :: Text -> Either P.ParseError Input
parse = P.parse (Input <$> workflows <*> (P.newline *> parts)) ""
  where
    workflows =
      P.sepEndBy workflow P.newline
        & mfilter (isJust . find ((== "in") . workflowLabel))
        & fmap (M.fromList . map (workflowLabel &&& id))
    workflow = do
      workflowLabel <- label
      _ <- P.char '{'
      conditionalRules <- P.sepEndBy (P.try conditionalRule) (P.char ',')
      unconditionalRule <- Unconditional <$> label
      _ <- P.char '}'
      pure Workflow {workflowLabel, conditionalRules, unconditionalRule}
    label = toText <$> P.many P.letter
    conditionalRule =
      Conditional <$> category <*> relation <*> int <*> (P.char ':' *> label)
    category = charToVariant [('x', X), ('m', M), ('a', A), ('s', S)]
    relation = charToVariant [('<', Lt), ('>', Gt)]
    charToVariant pairs = asum $ map (\(c, v) -> P.char c $> v) pairs
    parts = P.sepEndBy part P.newline
    part =
      P.between (P.char '{') (P.char '}') $
        P.sepBy1 rating (P.char ',') >>= \case
          [x, m, a, s] -> pure $ Part {x, m, a, s}
          _ -> fail "parts need all four categories"
    rating = P.letter *> P.char '=' *> int
    int = P.many P.digit >>= (maybe (fail "can't parse int") pure . readMaybe)

part1 :: Input -> Int
part1 (Input {workflows, parts}) = go "in" (one ("in", parts))
  where
    go key partsMap
      | M.keys partsMap == ["A", "R"] = partsMap M.! "A" & map totalRating & sum
      | key `elem` ["A", "R"] = go (nonTerminalKey partsMap) partsMap
      | otherwise =
          let workflow = workflows M.! key
           in go (nextWorkflow workflow) (passThrough workflow partsMap)

nonTerminalKey :: Map Label a -> Label
nonTerminalKey = fst . M.findMin . M.delete "A" . M.delete "R"

passThrough :: Workflow -> Map Label [Part] -> Map Label [Part]
passThrough workflow@(Workflow {workflowLabel}) partsMap =
  maybe rest sortParts partsToSort
  where
    (partsToSort, rest) = deleteLookup workflowLabel partsMap
    sortParts =
      M.unionWith (++) rest . buildMultiMap (applyWorkflow workflow)

buildMultiMap :: (Ord k) => (a -> k) -> [a] -> Map k [a]
buildMultiMap keyFunction = M.fromListWith (++) . map (keyFunction &&& one)

nextWorkflow :: Workflow -> Label
nextWorkflow (Workflow {conditionalRules = conds, unconditionalRule = uncond}) =
  conds & map goto & viaNonEmpty head & fromMaybe (label uncond)

deleteLookup :: (Ord k) => k -> Map k a -> (Maybe a, Map k a)
deleteLookup = M.updateLookupWithKey (\_ _ -> Nothing)

totalRating :: Part -> Int
totalRating (Part {x, m, a, s}) = x + m + a + s

applyWorkflow :: Workflow -> Part -> Label
applyWorkflow (Workflow {conditionalRules, unconditionalRule}) part =
  conditionalRules
    & map (matchRule part)
    & asum
    & fromMaybe (label unconditionalRule)

matchRule :: Part -> ConditionalRule -> Maybe Label
matchRule part (Conditional category relation amount label)
  | compareRatings relation (getCategory category part) amount = Just label
  | otherwise = Nothing

compareRatings :: Relation -> Rating -> Rating -> Bool
compareRatings Lt a b = a < b
compareRatings Gt a b = a > b

getCategory :: Category -> Part -> Int
getCategory X (Part {x}) = x
getCategory M (Part {m}) = m
getCategory A (Part {a}) = a
getCategory S (Part {s}) = s

part2 :: Input -> Int
part2 (Input {workflows}) = go start
  where
    go constraint@(Range {rangeLabel = "A"}) = combinations constraint
    go (Range {rangeLabel = "R"}) = 0
    go constraint@(Range {rangeLabel}) =
      sum $ map go (unconditional : conditionals)
      where
        unconditional = unconditional' {rangeLabel = label unconditionalRule}
        Workflow {conditionalRules, unconditionalRule} =
          workflows M.! rangeLabel
        (unconditional', conditionals) = mapAccumL f constraint conditionalRules
    f constraint condition = (constraint', applyCondition constraint condition)
      where
        constraint' = applyCondition constraint (invertCondition condition)
    start =
      Range
        { rangeLabel = "in",
          xRange = (1, 4000),
          mRange = (1, 4000),
          aRange = (1, 4000),
          sRange = (1, 4000)
        }

invertCondition :: ConditionalRule -> ConditionalRule
invertCondition (Conditional c Lt v l) = Conditional c Gt (v - 1) l
invertCondition (Conditional c Gt v l) = Conditional c Lt (v + 1) l

applyCondition :: Range -> ConditionalRule -> Range
applyCondition constraint@(Range {xRange}) condition@(Conditional X _ _ label) = constraint {xRange = narrowRange xRange condition, rangeLabel = label}
applyCondition constraint@(Range {mRange}) condition@(Conditional M _ _ label) = constraint {mRange = narrowRange mRange condition, rangeLabel = label}
applyCondition constraint@(Range {aRange}) condition@(Conditional A _ _ label) = constraint {aRange = narrowRange aRange condition, rangeLabel = label}
applyCondition constraint@(Range {sRange}) condition@(Conditional S _ _ label) = constraint {sRange = narrowRange sRange condition, rangeLabel = label}

narrowRange :: (Rating, Rating) -> ConditionalRule -> (Rating, Rating)
narrowRange (low, high) (Conditional _ Lt v _) = (low, min high (v - 1))
narrowRange (low, high) (Conditional _ Gt v _) = (max low (v + 1), high)

combinations :: Range -> Int
combinations (Range {xRange, mRange, aRange, sRange}) =
  product $ map f [xRange, mRange, aRange, sRange]
  where
    f (low, high) = high - low + 1
