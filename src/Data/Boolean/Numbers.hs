{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- There are warnings about this in the 'RealFracB' instances for 
-- 'Float' and 'Double'. They can be ignored.
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

-- -----------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean.Numbers
-- Copyright   :  (c) Jan Bracker 2013
-- License     :  BSD3
-- 
-- Maintainer  :  jbra@informatik.uni-kiel.de
-- Stability   :  experimental
-- 
-- A generalized version of the class hirarchy for numbers. All
-- functions that would break a potential deep embedding are removed
-- or generalized to support deep embeddings.
-- 
-- The class hirarchy for numeric types keeps as close as possible to the 
-- 'Prelude' hirarchy. A great part of the default implementation and comments
-- are copied and adopted from 'Prelude'.
-- -----------------------------------------------------------------------

module Data.Boolean.Numbers 
  ( IntegralB(..)
  , RealFracB(..)
  , RealFloatB(..)
  , evenB, oddB
  ) where

import Prelude as P
import Data.Boolean

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

infixr 9 .:
(.:) :: (c -> c') -> (a -> b -> c) -> (a -> b -> c')
(.:) = (.).(.)

(##) :: (a -> b -> c) -> (a -> b -> d) -> a -> b -> (c,d)
(f ## g) x y = (f x y, g x y)

viaZ :: (Integral a, Num b) => a -> b
viaZ = fromInteger . toInteger

-- Why not fromIntegral instead of fromInteger . toInteger ? (conal)
-- Why use it at all? (conal)

-- -----------------------------------------------------------------------
-- Generalized Number Class Hirarchy
-- -----------------------------------------------------------------------

-- | A deep embedded version of 'Integral'.
--   Integral numbers, supporting integer division.
--   
--   Minimal complete definition is either 'quotRem' and 'divMod'
--   or the other four functions.
class (Num a, OrdB a) => IntegralB a where
  -- | Integer division truncated towards zero.
  quot :: a -> a -> a
  quot = fst .: Data.Boolean.Numbers.quotRem
  -- | Integer reminder, satisfying:
  --   @(x `quot` y) * y + (x `rem` y) == x@
  rem :: a -> a -> a
  rem = snd .: Data.Boolean.Numbers.quotRem
  -- | Integer division truncated toward negative infinity.
  div :: a -> a -> a
  div = fst .: Data.Boolean.Numbers.divMod
  -- | Integer modulus, satisfying:
  --   @(x `div` y) * y + (x `mod` y) == x@
  mod :: a -> a -> a
  mod = snd .: Data.Boolean.Numbers.divMod
  -- | Simultaneous 'quot' and 'rem'.
  quotRem :: a -> a -> (a,a)
  quotRem = Data.Boolean.Numbers.quot ## Data.Boolean.Numbers.rem
  -- | Simultaneous 'div' and 'mod'.
  divMod :: a -> a -> (a,a)
  divMod  = Data.Boolean.Numbers.div  ## Data.Boolean.Numbers.mod

-- | Deep embedded version of 'RealFloat'.
--   Extracting components of fractions.
--   
--   Minimal complete definition: 'properFraction', 
--   'round', 'floor' and 'ceiling'.
class (Num a, OrdB a, Fractional a) => RealFracB a where
  -- | The function 'properFraction' takes a real fractional number @x@
  -- and returns a pair @(n,f)@ such that @x = n+f@, and:
  -- 
  -- * @n@ is an integral number with the same sign as @x@; and
  -- 
  -- * @f@ is a fraction with the same type and sign as @x@,
  --   and with absolute value less than @1@.
  --   
  -- The default definitions of the 'ceiling', 'floor', 'truncate'
  -- and 'round' functions are in terms of 'properFraction'.
  properFraction :: a -> (a, a)
  -- | @'truncate' x@ returns the integer nearest @x@ between zero and @x@
  truncate :: a -> a
  truncate = fst . Data.Boolean.Numbers.properFraction
  -- | @'round' x@ returns the nearest integer to @x@;
  --   the even integer if @x@ is equidistant between two integers
  round :: a -> a
  -- | @'ceiling' x@ returns the least integer not less than @x@
  ceiling :: a -> a
  -- | @'floor' x@ returns the greatest integer not greater than @x@.
  floor :: a -> a

-- | Deep embedded version of 'RealFloat'.
--   Efficient, machine-independent access to the components of a
--   floating-point number.
--   
--   A complete definition has to define all functions.
class (Boolean (BooleanOf a), RealFracB a, Floating a) => RealFloatB a where
  -- | 'true' if the argument is an IEEE \"not-a-number\" (NaN) value.
  isNaN :: a -> BooleanOf a
  -- | 'true' if the argument is an IEEE infinity or negative infinity.
  isInfinite :: a -> BooleanOf a
  -- | 'true' if the argument is an IEEE negative zero.
  isNegativeZero :: a -> BooleanOf a
  -- | 'true' if the argument is an IEEE floating point number.
  isIEEE :: a -> BooleanOf a
  -- | a version of arctangent taking two real floating-point arguments.
  --   For real floating @x@ and @y@, @'atan2' y x@ computes the angle
  --   (from the positive x-axis) of the vector from the origin to the
  --   point @(x,y)@.  @'atan2' y x@ returns a value in the range [@-pi@,
  --   @pi@].  It follows the Common Lisp semantics for the origin when
  --   signed zeroes are supported.  @'atan2' y 1@, with @y@ in a type
  --   that is 'RealFloatB', should return the same value as @'atan' y@.
  --   A default definition of 'atan2' is provided, but implementors
  --   can provide a more accurate implementation.
  atan2 :: a -> a -> a

-- -----------------------------------------------------------------------
-- Generalized Number Utility Functions
-- -----------------------------------------------------------------------

-- | Variant of 'even' for generalized booleans.
evenB :: (IfB a, EqB a, IntegralB a) => a -> BooleanOf a
evenB n = n `Data.Boolean.Numbers.rem` 2 ==* 0

-- | Variant of 'odd' for generalized booleans.
oddB :: (IfB a, EqB a, IntegralB a) => a -> BooleanOf a
oddB = notB . evenB

-- -----------------------------------------------------------------------
-- Default Class Instances for Basic Types
-- -----------------------------------------------------------------------

instance IntegralB Int where
  quotRem = P.quot ## P.rem
  divMod  = P.div  ## P.mod

instance IntegralB Integer where
  quotRem = P.quot ## P.rem
  divMod  = P.div  ## P.mod

-- Why not quotRem = P.quotRem etc? (conal)

instance RealFracB Float where
  properFraction x = (viaZ n, f) where (n, f) = P.properFraction x
  round   = viaZ . P.round
  floor   = viaZ . P.floor
  ceiling = viaZ . P.ceiling

instance RealFracB Double where
  properFraction x = (viaZ n, f) where (n, f) = P.properFraction x
  round   = viaZ . P.round
  floor   = viaZ . P.floor
  ceiling = viaZ . P.ceiling

instance RealFloatB Float where
  isNaN          = P.isNaN
  isInfinite     = P.isInfinite
  isNegativeZero = P.isNegativeZero
  isIEEE         = P.isIEEE
  atan2          = P.atan2

instance RealFloatB Double where
  isNaN          = P.isNaN
  isInfinite     = P.isInfinite
  isNegativeZero = P.isNegativeZero
  isIEEE         = P.isIEEE
  atan2          = P.atan2

