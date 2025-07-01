-- src/SymbolicPhysics/PrettyEL.hs
{-# LANGUAGE LambdaCase #-}

module SymbolicPhysics.PrettyEL
  ( prettyEL
  ) where

import           SymbolicPhysics.SymbolicD   ( Expr
                                             , simplify
                                             , pretty
                                             )

prettyEL
  :: [String] 
  -> (String, Expr) 
  -> String
prettyEL _ (coord, raw) =
  let simp = simplify raw
  in  "[" ++ coord ++ "]: " ++ pretty simp ++ " = 0"
