{-# LANGUAGE DeriveGeneric #-}

module SymbolicPhysics.SState
  ( SState(..)
  , emptySState
  , insertS
  , lookupS
  , keysS
  , fromListS
  ) where

import Components (Component)
import SymbolicPhysics.SymbolicD (Expr, constant)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

-- | Symbolic state: a mapping from components to symbolic expressions
newtype SState = SState (M.Map Component Expr)
  deriving (Eq, Show, Generic)

-- | Empty symbolic state
emptySState :: SState
emptySState = SState M.empty

-- | Insert or update a component's symbolic value
insertS :: Component -> Expr -> SState -> SState
insertS c v (SState m) = SState (M.insert c v m)

-- | Lookup a component's value, defaulting to 0 if missing
lookupS :: Component -> SState -> Expr
lookupS c (SState m) = M.findWithDefault (constant 0) c m

-- | Get all component keys in the state
keysS :: SState -> [Component]
keysS (SState m) = M.keys m

-- | Construct a state from a list of pairs
fromListS :: [(Component, Expr)] -> SState
fromListS xs = SState (M.fromList xs)
