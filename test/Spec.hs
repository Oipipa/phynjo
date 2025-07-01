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
import qualified Physics.ForceSpec
import qualified Physics.ForceNRSpec
import qualified Physics.ForceDSLSpec
import qualified System.SystemForcesSpec
import qualified Physics.Symplectic4Spec
import qualified Physics.RK4IntegratorSpec
import qualified Physics.RigidBodySpec
import qualified Physics.RigidStateSpec
import qualified Physics.ContactSpec
import qualified Physics.SymbolicSpec
import qualified SymbolicPhysics.SStateSpec
import qualified SymbolicPhysics.SRuneSpec
import qualified SymbolicPhysics.SSpellSpec

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
  Physics.ForceSpec.spec
  Physics.ForceNRSpec.spec
  Physics.ForceDSLSpec.spec
  System.SystemForcesSpec.spec
  Physics.Symplectic4Spec.spec
  Physics.RK4IntegratorSpec.spec
  Physics.RigidBodySpec.spec
  Physics.RigidStateSpec.spec
  Physics.ContactSpec.spec
  Physics.SymbolicSpec.spec
  SymbolicPhysics.SStateSpec.spec
  SymbolicPhysics.SRuneSpec.spec
  SymbolicPhysics.SSpellSpec.spec
