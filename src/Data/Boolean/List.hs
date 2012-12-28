{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean.List
-- License     :  BSD3
-- 
-- Author      :  Jan Bracker (jbracker)
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- 
-- Generalized versions of Data.List functions using booleans.
---------------------------------------------------------------------- 

module Data.Boolean.List
  ( andB, orB
  , anyB, allB
  , maximumB, minimumB
  ) where

import Data.Boolean

filterB :: (Boolean (BooleanOf a), IfB a) => (a -> BooleanOf a) -> [a] -> [a]
filterB _ [] = []
filterB p (x:xs) = ifB (p x) (x : filterB p xs) (filterB p xs)

-- | Returns the conjunction of the booleans in the list.
--   Generalization of 'Data.List.and' with the same behaviour on infinite lists.
andB :: (Boolean (BooleanOf a)) => [BooleanOf a] -> BooleanOf a
andB = foldr (&&*) true

-- | Returns the disjunction of the booleans in the list.
--   Generalization of 'Data.List.or' with the same behaviour on infinite lists.
orB :: (Boolean (BooleanOf a)) => [BooleanOf a] -> BooleanOf a
orB = foldr (||*) false

-- | Returns if there exists an element that satisfies the predicate.
--   Generalization of 'Data.List.any' with the same behaviour on infinite lists.
anyB :: (Boolean (BooleanOf a)) => (a -> BooleanOf a) -> [a] -> BooleanOf a
anyB _ [] = false
anyB p (x:xs) = p x ||* anyB p xs

-- | Returns if all elements of the list satisfy the predicate.
--   Generalization of 'Data.List.all' with the same behaviour on infinite lists.
allB :: (Boolean (BooleanOf a)) => (a -> BooleanOf a) -> [a] -> BooleanOf a
allB _ [] = true
allB p (x:xs) = p x &&* allB p xs

-- | Returns the maximum value of a non-empty, finite list.
--   Generalization of 'Data.List.maximum'.
maximumB :: (IfB a, OrdB a) => [a] -> a
maximumB [] = error "maximumB: empty list"
maximumB xs = foldl1 maxB xs

-- | Returns the minimum value of a non-empty, finite list.
--   Generalization of 'Data.List.minimum'.
minimumB :: (IfB a, OrdB a) => [a] -> a
minimumB [] = error "minimumB: empty list"
minimumB xs = foldl1 minB xs