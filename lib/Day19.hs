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
  )
where

import qualified Data.Map as M
import qualified Text.Parsec as P

data Input = Input
  { workflows :: [Workflow],
    parts :: [Part]
  }
  deriving (Show, Eq)

type Label = Text

data Workflow = Workflow
  { name :: Label,
    conditionalRules :: [ConditionalRule],
    unconditionalRule :: UnconditionalRule
  }
  deriving (Show, Eq)

data ConditionalRule
  = Conditional Category Relation Int Label
  deriving (Show, Eq)

newtype UnconditionalRule = Unconditional {label :: Label} deriving (Show, Eq)

data Category = X | M | A | S deriving (Show, Eq)

data Relation = Lt | Gt deriving (Show, Eq)

type Rating = Int

data Part = Part {x :: Rating, m :: Rating, a :: Rating, s :: Rating}
  deriving (Show, Eq)

data PartConstraint = PartConstraint
  { position :: Label,
    xConstraint :: (Rating, Rating),
    mConstraint :: (Rating, Rating),
    aConstraint :: (Rating, Rating),
    sConstraint :: (Rating, Rating)
  }

parse :: Text -> Either P.ParseError Input
parse = P.parse (Input <$> workflows <*> (P.newline *> parts)) ""
  where
    workflows =
      P.sepEndBy workflow P.newline & mfilter (isJust . find ((== "in") . name))
    workflow = do
      name <- label
      _ <- P.char '{'
      conditionalRules <- P.sepEndBy (P.try conditionalRule) (P.char ',')
      unconditionalRule <- Unconditional <$> label
      _ <- P.char '}'
      pure Workflow {name, conditionalRules, unconditionalRule}
    label = toText <$> P.many P.letter
    conditionalRule =
      Conditional <$> category <*> relation <*> int <*> (P.char ':' *> label)
    category =
      asum [P.char 'x' $> X, P.char 'm' $> M, P.char 'a' $> A, P.char 's' $> S]
    relation = asum [P.char '<' $> Lt, P.char '>' $> Gt]
    parts = P.sepEndBy part P.newline
    part =
      P.between
        (P.char '{')
        (P.char '}')
        (P.sepBy1 rating (P.char ',') >>= listToQuad Part)
    listToQuad f [t, u, v, w] = pure $ f t u v w
    listToQuad _ _ = fail "not four of the thing"
    rating = P.letter *> P.char '=' *> int
    int = P.many P.digit >>= (maybe (fail "can't parse int") pure . readMaybe)

part1 :: Input -> Int
part1 (Input {workflows, parts}) = go "in" (one ("in", parts))
  where
    go key partsMap
      | M.keys partsMap == ["A", "R"] = partsMap M.! "A" & map totalRating & sum
      | key `elem` ["A", "R"] =
          go
            (fst $ M.findMin $ M.filterWithKey (\k _ -> k `notElem` ["A", "R"]) partsMap)
            partsMap
      | otherwise =
          let (partsToSort, rest) = deleteLookup key partsMap
              workflow = workflows' M.! key
              partsMap' = maybe rest (sortParts rest workflow) partsToSort
           in go (nextWorkflow workflow) partsMap'
    sortParts rest workflow =
      M.unionWith (++) rest . M.fromListWith (++) . map (applyWorkflow workflow &&& one)
    workflows' = M.fromList $ map (name &&& id) workflows

nextWorkflow :: Workflow -> Label
nextWorkflow (Workflow {conditionalRules, unconditionalRule}) =
  conditionalRules
    & map (\(Conditional _ _ _ l) -> l)
    & viaNonEmpty head
    & fromMaybe (label unconditionalRule)

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
    go constraint@(PartConstraint {position = "A"}) = combinations constraint
    go (PartConstraint {position = "R"}) = 0
    go constraint@(PartConstraint {position}) =
      let Workflow {conditionalRules, unconditionalRule} = workflows' M.! position
          (unconditional, conditionals) = mapAccumL f constraint conditionalRules
       in go (unconditional {position = label unconditionalRule}) + sum (map go conditionals)
    f constraint condition =
      let constraint' = applyCondition constraint (invertCondition condition)
       in (constraint', applyCondition constraint condition)
    start =
      PartConstraint
        { position = "in",
          xConstraint = (1, 4000),
          mConstraint = (1, 4000),
          aConstraint = (1, 4000),
          sConstraint = (1, 4000)
        }
    workflows' = M.fromList $ map (name &&& id) workflows

invertCondition :: ConditionalRule -> ConditionalRule
invertCondition (Conditional c Lt v l) = Conditional c Gt (v - 1) l
invertCondition (Conditional c Gt v l) = Conditional c Lt (v + 1) l

applyCondition :: PartConstraint -> ConditionalRule -> PartConstraint
applyCondition constraint@(PartConstraint {xConstraint}) condition@(Conditional X _ _ label) = constraint {xConstraint = narrowRange xConstraint condition, position = label}
applyCondition constraint@(PartConstraint {mConstraint}) condition@(Conditional M _ _ label) = constraint {mConstraint = narrowRange mConstraint condition, position = label}
applyCondition constraint@(PartConstraint {aConstraint}) condition@(Conditional A _ _ label) = constraint {aConstraint = narrowRange aConstraint condition, position = label}
applyCondition constraint@(PartConstraint {sConstraint}) condition@(Conditional S _ _ label) = constraint {sConstraint = narrowRange sConstraint condition, position = label}

narrowRange :: (Rating, Rating) -> ConditionalRule -> (Rating, Rating)
narrowRange (low, high) (Conditional _ Lt v _) = (low, min high (v - 1))
narrowRange (low, high) (Conditional _ Gt v _) = (max low (v + 1), high)

combinations (PartConstraint {xConstraint, mConstraint, aConstraint, sConstraint}) =
  product $ map f [xConstraint, mConstraint, aConstraint, sConstraint]
  where
    f (low, high) = high - low + 1
