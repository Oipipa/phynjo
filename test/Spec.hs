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
import qualified UnitLiteralSpec
import qualified BodySpec
import qualified SystemSpec
import qualified ScalarLitSpec
import qualified DriftNRSpec
import qualified EulerNRSpec

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
  UnitLiteralSpec.spec
  BodySpec.spec
  SystemSpec.spec
  ScalarLitSpec.spec
  EulerNRSpec.spec
