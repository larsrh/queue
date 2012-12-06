module Common (
  NonEmptyList,
  nubNE
) where

import Data.List (nub)
import qualified Data.NonEmpty as NE

type NonEmptyList a = NE.T [] a

nubNE :: Eq a => NonEmptyList a -> NonEmptyList a
nubNE (NE.Cons x xs) = aux $ nub xs
  where aux [] = NE.Cons x []
        aux (y : ys) | x == y || x `elem` ys = NE.Cons y ys
        aux ys = NE.Cons x ys

-- vim: expandtab:ts=2:sw=2
