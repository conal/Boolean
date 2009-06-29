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

module Data.Boolean
  (
    Boolean(..),IfB(..), boolean, cond, crop
  , EqB(..), OrdB(..)
  ) where

import Data.Monoid (Monoid,mempty)
import Control.Applicative (Applicative(..),liftA2,liftA3)


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
class Boolean b => IfB b a | a -> b where
  ifB  :: b -> a -> a -> a

-- | Expression-lifted conditional with condition last
boolean :: IfB b a => a -> a -> b -> a
boolean t e b = ifB b t e

-- | Point-wise conditional
cond :: (Applicative f, IfB b a) => f b -> f a -> f a -> f a
cond = liftA3 ifB

-- | Crop a function, filling in 'mempty' where the test yeis false.
crop :: (Applicative f, Monoid (f a), IfB bool a) => f bool -> f a -> f a
crop r f = cond r f mempty


infix  4  ==*, /=*

-- | Types with equality.  Minimum definition: '(==*)'.
class Boolean b => EqB b a | a -> b where
  (==*), (/=*) :: a -> a -> b
  u /=* v = notB (u ==* v)

infix  4  <*, <=*, >=*, >*

-- | Types with inequality.  Minimum definition: '(<*)'.
class Boolean b => OrdB b a | a -> b where
  (<*), (<=*), (>*), (>=*) :: a -> a -> b
  u >*  v = v <* u
  u >=* v = notB (u <* v)
  u <=* v = v >=* u

{--------------------------------------------------------------------
    Some instances
--------------------------------------------------------------------}

ife :: Bool -> a -> a -> a
ife c t e = if c then t else e

-- I'd give the following instances:
-- 
--     instance IfB  Bool a where ifB = ife
--     instance EqB  Bool a where { (==*) = (==) ; (/=*) = (/=) }
--     instance OrdB Bool a where { (<*) = (<) ; (<=*) = (<=)}
-- 
-- Sadly, doing so would break the a->b fundep, which is needed elsewhere
-- for disambiguation.  So use the instances above as templates, filling
-- in specific types for a.


instance IfB  Bool Float where ifB = ife
instance EqB  Bool Float where { (==*) = (==) ; (/=*) = (/=) }
instance OrdB Bool Float where { (<*) = (<) ; (<=*) = (<=) }

instance IfB Bool [a] where ifB = ife

-- Similarly for other types.  


instance (IfB b p, IfB b q) => IfB b (p,q) where
  ifB w (p,q) (p',q') = (ifB w p p', ifB w q q')

instance (IfB b p, IfB b q, IfB b r) => IfB b (p,q,r) where
  ifB w (p,q,r) (p',q',r') = (ifB w p p', ifB w q q', ifB w r r')

instance (IfB b p, IfB b q, IfB b r, IfB b s) => IfB b (p,q,r,s) where
  ifB w (p,q,r,s) (p',q',r',s') =
    (ifB w p p', ifB w q q', ifB w r r', ifB w s s')



-- Standard pattern for applicative functors:

instance Boolean b => Boolean (z -> b) where
  true  = pure true
  false = pure false
  notB  = fmap notB
  (&&*) = liftA2 (&&*)
  (||*) = liftA2 (||*)

instance IfB b a => IfB (z -> b) (z -> a) where
  ifB = cond

instance EqB  b a => EqB  (z -> b) (z -> a) where
  { (==*) = liftA2 (==*) ; (/=*) = liftA2 (/=*) }
instance OrdB b a => OrdB (z -> b) (z -> a) where
  { (<*) = liftA2(<*) ; (<=*) = liftA2(<=*) }


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
