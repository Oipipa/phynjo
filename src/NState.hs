{-# LANGUAGE NamedFieldPuns #-}

module NState
  ( NState(..)
  , emptyNS
  , insertPos , insertMom
  , lookupPos , lookupMom
  , componentsNS
  ) where

import Components        (Component)
import ScalarLiteral
  ( SLit , emptySL , insertSL , lookupSL , keysSL )

----------------------------------------------------------------------
-- Numeric world-state: 1-D positions & momenta stored in SLit
----------------------------------------------------------------------

data NState = NS
  { q :: SLit    -- ^ positions  (metres, plain Double)
  , p :: SLit    -- ^ momenta    (kg·m/s, plain Double)
  }
  deriving (Eq, Show)

emptyNS :: NState
emptyNS = NS emptySL emptySL

-- | Update / insert position
insertPos :: Component -> Double -> NState -> NState
insertPos c x ns@NS{q} = ns { q = insertSL c x q }

-- | Update / insert momentum
insertMom :: Component -> Double -> NState -> NState
insertMom c v ns@NS{p} = ns { p = insertSL c v p }

lookupPos :: Component -> NState -> Double
lookupPos c NS{q} = lookupSL c q

lookupMom :: Component -> NState -> Double
lookupMom c NS{p} = lookupSL c p

componentsNS :: NState -> [Component]
componentsNS NS{q} = keysSL q
