module Machine (
  Machine,
  Operation,
  accepts,
  match,
  select,
  compress,
  successful,
  runUntilFinal,
  runSuccess,
  runFirst,
  runRandom,
  runAll
) where

import Control.Monad.Identity
import Data.Maybe (catMaybes)
import qualified Data.NonEmpty as NE
import Data.Random
import qualified Data.Random.Extras as Random

import Common

class Eq state => Machine rule state | state -> rule where
  accepts :: state -> Bool
  match :: state -> rule -> Maybe state

data Operation m = Operation {
  select :: forall r. NonEmptyList r -> m r,
  compress :: forall s. Eq s => m s -> m s,
  successful :: m Bool -> IO Bool
}

flipFilter :: (Monad m, Machine rule state) => Either Bool (m state) -> m (Either Bool state)
flipFilter (Left b) = return $ Left b
flipFilter (Right ms) = do s <- ms
                           return $ if accepts s then Left True else Right s

runStep :: (Monad m, Machine rule state) => Operation m -> [rule] -> state -> m (Either Bool state)
runStep op rules s = flipFilter $ apply s $ catMaybes $ map (match s) rules
  where apply s [] = Left $ accepts s
        apply _ (r : rs) = Right $ select op $ NE.Cons r rs

runUntilFinal :: (Monad m, Machine rule state) => Operation m -> [rule] -> state -> m Bool
runUntilFinal op rules s = do ebs <- compress op $ runStep op rules s
                              case ebs of
                                Left b -> return b
                                Right s' -> runUntilFinal op rules s'

runSuccess :: (Monad m, Machine rule state) => Operation m -> [rule] -> state -> IO Bool
runSuccess op rules s = successful op $ runUntilFinal op rules s

runFirst :: Operation Identity
runFirst = Operation {
  select = Identity . NE.head,
  compress = id,
  successful = return . runIdentity
}

runRandom :: Operation RVar
runRandom = Operation {
  select = Random.choice . NE.flatten,
  compress = id,
  successful = flip runRVarT StdRandom
}

runAll :: Operation (NE.T [])
runAll = Operation {
  select = id,
  compress = nubNE,
  successful = return . NE.foldl1 (||)
}

-- vim: expandtab:ts=2:sw=2
