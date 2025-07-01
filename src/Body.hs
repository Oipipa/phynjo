{-# LANGUAGE DataKinds #-}

module Body
  ( Body(..)
  , mkBody
  ) where

import Components   (Component(..))
import UnitLiteral
import Numeric.Units.Dimensional.Prelude
         ( Quantity, (*~), metre, second, kilo, gram
         , DLength, DMass )
import qualified Numeric.Units.Dimensional.Prelude as D

data Body = Body
  { ident :: Component   -- ^ unique identifier
  , mass  :: MassLit     -- ^ mass   (kg)
  , pos0  :: PosLit      -- ^ position (m)
  , mom0  :: MomLit      -- ^ momentum (kg·m/s)
  }
  deriving (Eq, Show)

mkBody
  :: String                  -- ^ label
  -> Quantity DMass Double   -- ^ mass  (e.g. 1 *~ kilo gram)
  -> Double                  -- ^ x-coordinate (metres)
  -> Body
mkBody label m x =
  let c     = AtomicC label
      zeroP = 0 *~ (kilo gram D.* metre D./ second)  -- 0 kg·m/s
  in Body
       { ident = c
       , mass  = singletonU c m
       , pos0  = singletonU c (x *~ metre)
       , mom0  = singletonU c zeroP
       }
