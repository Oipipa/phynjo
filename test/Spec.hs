module Main (main) where

import Test.Hspec (hspec)

import qualified ComponentsSpec
import qualified LiteralSpec
import qualified TransitionSpec
import qualified ActionSpec
import qualified ProcessSpec

-- renamed:
import qualified EventRuleSpec
import qualified EventWorkflowSpec

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
import qualified Physics.Force3D.ExtraSpec
import qualified Physics.Contact.FlexibleSpec
import qualified Physics.Math.Vec3UtilSpec
import qualified Physics.Sim.ComposeSpec

main :: IO ()
main = hspec $ do
  ComponentsSpec.spec
  LiteralSpec.spec
  TransitionSpec.spec
  ActionSpec.spec
  ProcessSpec.spec

  EventRuleSpec.spec
  EventWorkflowSpec.spec

  ConstraintSpec.spec
  UnitLiteralSpec.spec
  BodySpec.spec
  SystemSpec.spec
  ScalarLitSpec.spec

  DriftNRSpec.spec
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
  Physics.Force3D.ExtraSpec.spec
  Physics.Contact.FlexibleSpec.spec
  Physics.Math.Vec3UtilSpec.spec
  Physics.Sim.ComposeSpec.spec
