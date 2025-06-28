module Main (main) where

import Test.Hspec (hspec)

import qualified ComponentsSpec
import qualified LiteralSpec
import qualified TransitionSpec
import qualified ActionSpec
import qualified ProcessSpec
import qualified RuneSpec
import qualified SpellSpec
import qualified ConstraintSpec

main :: IO ()
main = hspec $ do
  ComponentsSpec.spec
  LiteralSpec.spec
  TransitionSpec.spec
  ActionSpec.spec
  ProcessSpec.spec
  RuneSpec.spec
  SpellSpec.spec
  ConstraintSpec.spec
