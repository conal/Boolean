{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
           , UndecidableInstances, ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some classes for generalized boolean operations.
-- 
-- In this design, for if-then-else, equality and inequality tests, the
-- boolean type depends functionally on the value type.  This dependency
-- allows the boolean type to be inferred in a conditional expression.
-- 
-- I also tried using a unary type constructor class.  The class doesn't work
-- for regular booleans, so generality is lost.  Also, we'd probably have
-- to wire class constraints in like: @(==*) :: Eq a => f Bool -> f a -> f
-- a -> f a@, which disallows situations needing additional constraints,
-- e.g., Show.
-- 
----------------------------------------------------------------------

module Data.Boolean
  (
    Boolean(..),IfB(..), boolean, cond, crop
  , EqB(..), OrdB(..), minB, maxB
  ) where

import Data.Monoid (Monoid,mempty)
import Control.Applicative (Applicative(pure),liftA2,liftA3)


{--------------------------------------------------------------------
    Classes
--------------------------------------------------------------------}

infixr 3  &&*
infixr 2  ||*

-- | Generalized boolean class
class Boolean b where
  true, false  :: b
  notB         :: b -> b
  (&&*), (||*) :: b -> b -> b

instance Boolean Bool where
  true  = True
  false = False
  notB  = not
  (&&*) = (&&)
  (||*) = (||)

-- | Types with conditionals
class Boolean bool => IfB bool a | a -> bool where
  ifB  :: bool -> a -> a -> a

-- | 'ifB' with condition last
boolean :: IfB bool a => a -> a -> bool -> a
boolean t e bool = ifB bool t e

-- | Point-wise conditional
cond :: (Applicative f, IfB bool a) => f bool -> f a -> f a -> f a
cond = liftA3 ifB

-- | Generalized cropping, filling in 'mempty' where the test yields false.
crop :: (Applicative f, Monoid (f a), IfB bool a) => f bool -> f a -> f a
crop r f = cond r f mempty

infix  4  ==*, /=*

-- | Types with equality.  Minimum definition: '(==*)'.
class Boolean bool => EqB bool a | a -> bool where
  (==*), (/=*) :: a -> a -> bool
  u /=* v = notB (u ==* v)

infix  4  <*, <=*, >=*, >*

-- | Types with inequality.  Minimum definition: '(<*)'.
class Boolean bool => OrdB bool a | a -> bool where
  (<*), (<=*), (>*), (>=*) :: a -> a -> bool
  u >*  v = v <* u
  u >=* v = notB (u <* v)
  u <=* v = v >=* u

-- | Variant of 'min' using 'ifB' and '(<=*)'
minB :: (IfB bool a, OrdB bool a) => a -> a -> a
u `minB` v = ifB (u <=* v) u v

-- | Variant of 'max' using 'ifB' and '(>=*)'
maxB :: (IfB bool a, OrdB bool a) => a -> a -> a
u `maxB` v = ifB (u >=* v) u v

{--------------------------------------------------------------------
    Some instances
--------------------------------------------------------------------}

ife :: Bool -> a -> a -> a
ife c t e = if c then t else e

-- I'd give the following instances:
-- 
--     instance          IfB  Bool a where ifB = ife
--     instance Eq  a => EqB  Bool a where { (==*) = (==) ; (/=*) = (/=) }
--     instance Ord a => OrdB Bool a where { (<*) = (<) ; (<=*) = (<=)}
-- 
-- Sadly, doing so would break the a->bool fundep, which is needed elsewhere
-- for disambiguation.  So use the instances above as templates, filling
-- in specific types for a.


instance IfB  Bool Float where ifB = ife
instance EqB  Bool Float where { (==*) = (==) ; (/=*) = (/=) }
instance OrdB Bool Float where { (<*) = (<) ; (<=*) = (<=) }

-- Similarly for other types.  

instance (IfB bool p, IfB bool q) => IfB bool (p,q) where
  ifB w (p,q) (p',q') = (ifB w p p', ifB w q q')

instance (IfB bool p, IfB bool q, IfB bool r) => IfB bool (p,q,r) where
  ifB w (p,q,r) (p',q',r') = (ifB w p p', ifB w q q', ifB w r r')

instance (IfB bool p, IfB bool q, IfB bool r, IfB bool s) => IfB bool (p,q,r,s) where
  ifB w (p,q,r,s) (p',q',r',s') =
    (ifB w p p', ifB w q q', ifB w r r', ifB w s s')

-- Standard pattern for applicative functors:

instance Boolean bool => Boolean (z -> bool) where
  true  = pure true
  false = pure false
  notB  = fmap notB
  (&&*) = liftA2 (&&*)
  (||*) = liftA2 (||*)

instance IfB bool a => IfB (z -> bool) (z -> a) where
  ifB = cond

instance EqB  bool a => EqB  (z -> bool) (z -> a) where
  { (==*) = liftA2 (==*) ; (/=*) = liftA2 (/=*) }
instance OrdB bool a => OrdB (z -> bool) (z -> a) where
  { (<*) = liftA2 (<*) ; (<=*) = liftA2 (<=*) }


{-

{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

t1 :: String
t1 = ifB True "foo" "bar"

t2 :: Float -> Float
t2 = ifB (< 0) negate id

--     No instance for (IfB (a -> Bool) (a1 -> a1))
--       arising from a use of `ifB'
-- 
-- t2 = ifB (< 0) negate id                -- abs

-}
