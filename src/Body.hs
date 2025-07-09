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
  { ident :: Component 
  , mass  :: MassLit 
  , pos0  :: PosLit 
  , mom0  :: MomLit 
  }
  deriving (Eq, Show)

mkBody
  :: String 
  -> Quantity DMass Double 
  -> Double 
  -> Body
mkBody label m x =
  let c     = AtomicC label
      zeroP = 0 *~ (kilo gram D.* metre D./ second) 
  in Body
       { ident = c
       , mass  = singletonU c m
       , pos0  = singletonU c (x *~ metre)
       , mom0  = singletonU c zeroP
       }
