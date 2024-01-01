{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day20 (parse, part1) where

import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Text.Parsec as P

data Pulse = Low | High deriving (Show, Eq)

type ModuleName = Text

data ModuleType
  = FlipFlop Pulse
  | Conjunction (Map ModuleName Pulse)
  | Broadcast
  deriving (Show, Eq)

data Module = Module
  { moduleType :: ModuleType,
    moduleName :: ModuleName,
    outputs :: [ModuleName]
  }
  deriving (Show, Eq)

data Modules = Modules
  { broadcastModule :: Module,
    otherModules :: Map ModuleName Module
  }
  deriving (Show, Eq)

data PulseCounter = PulseCounter {lows :: Int, highs :: Int} deriving (Show, Eq)

instance Semigroup PulseCounter where
  PulseCounter lows1 highs1 <> PulseCounter lows2 highs2 =
    PulseCounter {lows = lows1 + lows2, highs = highs1 + highs2}

instance Monoid PulseCounter where
  mempty = PulseCounter {lows = 0, highs = 0}

parse :: Text -> Either P.ParseError Modules
parse = P.parse modules ""
  where
    modules = do
      ms <- P.sepEndBy communicationModule P.newline
      case second (`keyedBy` moduleName) $ partition isBroadcast ms of
        ([broadcastModule], otherModules) ->
          pure $ initialise $ Modules {broadcastModule, otherModules}
        ([], _) -> fail "no broadcast module"
        _ -> fail "more than one broadcast module"
    communicationModule =
      genericModule pass (const (Module Broadcast "broadcaster"))
        <|> genericModule (P.char '%') (Module (FlipFlop Low))
        <|> genericModule (P.char '&') (Module (Conjunction mempty))
    genericModule prefix out =
      out <$> (prefix *> name) <*> (P.string " -> " *> outputs)
    outputs = P.sepBy name (P.string ", ")
    name = toText <$> P.many P.letter

keyedBy :: (Ord k) => [a] -> (a -> k) -> Map k a
keyedBy elems keyFunction = M.fromList $ map (keyFunction &&& id) elems

initialise :: Modules -> Modules
initialise (Modules {broadcastModule, otherModules}) =
  Modules {broadcastModule, otherModules = M.foldr f otherModules conjunctions}
  where
    f m@(Module {moduleName = name}) =
      M.insert name m {moduleType = Conjunction (inputsOfConjunction name)}
    inputsOfConjunction name =
      M.fromList [(moduleName m, Low) | m <- allModules, hasOutput name m]
    allModules = broadcastModule : toList otherModules
    hasOutput outputName (Module {outputs}) = outputName `elem` outputs
    conjunctions = M.filter isConjunction otherModules

isConjunction :: Module -> Bool
isConjunction (Module {moduleType = Conjunction _}) = True
isConjunction _ = False

isBroadcast :: Module -> Bool
isBroadcast (Module {moduleType = Broadcast}) = True
isBroadcast _ = False

part1 :: Modules -> Int
part1 = finalProduct . fst . iterateNM 1000 pressButton

iterateNM :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateNM 0 _ a = pure [a]
iterateNM n f a = f a >>= fmap (a :) . iterateNM (n - 1) f

finalProduct :: PulseCounter -> Int
finalProduct (PulseCounter {lows, highs}) = lows * highs

pressButton :: Modules -> (PulseCounter, Modules)
pressButton modules =
  emit Low *> handlePulses (one ("broadcaster", ("button", Low))) modules

handlePulses :: Seq (ModuleName, (ModuleName, Pulse)) -> Modules -> (PulseCounter, Modules)
handlePulses S.Empty modules = pure modules
handlePulses (("broadcaster", pulse) S.:<| pulses) modules@(Modules {broadcastModule}) = do
  let (broadcast', pulses') = sendPulse broadcastModule pulse
  countPulses pulses'
  handlePulses
    (pulses <> S.fromList (pulses' `from` "broadcaster"))
    (modules {broadcastModule = broadcast'})
handlePulses ((destination, pulse) S.:<| pulses) modules@(Modules {otherModules}) =
  case otherModules M.!? destination of
    Just targetModule -> do
      let (module', pulses') = sendPulse targetModule pulse
      countPulses pulses'
      handlePulses
        (pulses <> S.fromList (pulses' `from` destination))
        (modules {otherModules = M.insert destination module' otherModules})
    Nothing -> handlePulses pulses modules

from :: [(ModuleName, Pulse)] -> ModuleName -> [(ModuleName, (ModuleName, Pulse))]
from xs source = let (ds, ps) = unzip xs in zip ds (map (source,) ps)

countPulses :: [(ModuleName, Pulse)] -> (PulseCounter, ())
countPulses = foldMap (emit . snd)

emit :: Pulse -> (PulseCounter, ())
emit Low = (mempty {lows = 1}, ())
emit High = (mempty {highs = 1}, ())

sendPulse :: Module -> (ModuleName, Pulse) -> (Module, [(ModuleName, Pulse)])
sendPulse m@(Module {moduleType = Broadcast, outputs}) (_, pulse) =
  (m, pulse `toAll` outputs)
sendPulse m@(Module {moduleType = FlipFlop _}) (_, High) = (m, [])
sendPulse m@(Module {moduleType = FlipFlop pulse, outputs}) (_, Low) =
  (m {moduleType = FlipFlop (invert pulse)}, invert pulse `toAll` outputs)
sendPulse m@(Module {moduleType = Conjunction memory, outputs}) (source, pulse) =
  let memory' = M.insert source pulse memory
      pulse' = if all isHigh memory' then Low else High
   in (m {moduleType = Conjunction memory'}, pulse' `toAll` outputs)

isHigh :: Pulse -> Bool
isHigh Low = False
isHigh High = True

invert :: Pulse -> Pulse
invert Low = High
invert High = Low

toAll :: Pulse -> [ModuleName] -> [(ModuleName, Pulse)]
toAll pulse = map (,pulse)
