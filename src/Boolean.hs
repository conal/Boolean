{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
           , UndecidableInstances, ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Boolean
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some classes for generalized boolean operations.
-- 
-- This design is very flexible but can require a lot of type annotation.
-- Some other possibilities:
-- 
-- + Unary type constructor class.  Then doesn't work for regular booleans,
--   so generality is lost.  Also, we'd probably have to wire class
--   constraints in like: @(==*) :: Eq a => f Bool -> f a -> f a -> f a@,
--   which disallows situations needing additional constraints, e.g., Show.
--   
-- + Functional dependency (or type family), with the bool type depending
--   on the value type.  Then the general Bool instances are invalid.  The
--   other dependency wouldn't do, since these functions are polymorphic.
-- 
----------------------------------------------------------------------

module Boolean (Boolean(..), EqB(..), OrdB(..), (>*), (>=*)) where

import Control.Applicative (liftA2,liftA3)


{--------------------------------------------------------------------
    Classes
--------------------------------------------------------------------}

class Boolean b a | a -> b where
  ifB  :: b -> a -> a -> a

class Boolean b a => EqB b a where
  (==*), (/=*) :: a -> a -> b

class Boolean b a => OrdB b a where
  (<*), (<=*) :: a -> a -> b

(>*), (>=*) :: OrdB b a => a -> a -> b
(>*)  = flip (<*)
(>=*) = flip (<=*)

{--------------------------------------------------------------------
    Some instances
--------------------------------------------------------------------}

ife :: Bool -> a -> a -> a
ife c t e = if c then t else e

-- I'd give the following instances:
-- 
--     instance Boolean Bool a where ifB = ife
--     instance EqB     Bool a where (==*) = (==)
--     instance OrdB    Bool a where
--       { (<*)=(<);(>*)=(>);(<=*)=(<=);(>=*)=(>=);}
-- 
-- Sadly, doing so would break the a->b fundep, which is needed elsewhere
-- for disambiguation.  So use the instances above as templates, filling
-- in specific types for a.

instance Boolean Bool Float where ifB = ife
instance EqB     Bool Float where { (==*) = (==) ; (==*) = (==) }
instance OrdB    Bool Float where { (<*)=(<) ; (<=*)=(<=) }

instance Boolean Bool [a] where ifB = ife



-- Similarly for other types.  


instance Boolean b a => Boolean (z -> b) (z -> a) where
  ifB = liftA3 ifB

instance EqB  b a => EqB  (z -> b) (z -> a) where (==*) = liftA2 (==*)
instance OrdB b a => OrdB (z -> b) (z -> a) where
  { (<*)=liftA2(<*) ; (<=*)=liftA2(<=*) }


{-

{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

t1 :: String
t1 = ifB True "foo" "bar"

t2 :: Float -> Float
t2 = ifB (< 0) negate id

--     No instance for (Boolean (a -> Bool) (a1 -> a1))
--       arising from a use of `ifB'
-- 
-- t2 = ifB (< 0) negate id                -- abs

-}
