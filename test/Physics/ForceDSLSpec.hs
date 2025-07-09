{-# LANGUAGE OverloadedStrings #-}

module Physics.ForceDSLSpec (spec) where

import Test.Hspec
import Control.Exception       (evaluate)

import Physics.Forces.ForceDSL        (addForce, addForces)
import Physics.Forces.Force           (Force(..), (<+>))
import NumericWorkflow         (workflowDomain)
import Components              (Component(AtomicC))
import Body                     (mkBody)
import System.SystemForces      (mkSystem)
import Numeric.Units.Dimensional.Prelude
  ( (*~), kilo, gram )

import qualified Data.Set      as S

spec :: Spec
spec = describe "Physics.ForceDSL (leapfrog1D)" $ do
  let -- two bodies “a” and “b”
      b1  = mkBody "a" (1 *~ kilo gram) 0
      b2  = mkBody "b" (1 *~ kilo gram) 1
      sys = mkSystem [b1, b2]

      a   = AtomicC "a"
      b   = AtomicC "b"
      dt  = 0.1

      -- two dummy custom forces
      f1, f2 :: Force
      f1 = Custom $ \_ _ -> (1,0,0)
      f2 = Custom $ \_ _ -> (2,0,0)

  it "addForce uses leapfrog and covers all bodies" $ do
    let wf = addForce dt f1 sys
    workflowDomain wf `shouldBe` S.fromList [a,b]

  it "addForces on one force equals addForce" $ do
    let wf1 = addForces dt [f1] sys
        wf2 = addForce  dt  f1  sys
    workflowDomain wf1 `shouldBe` workflowDomain wf2

  it "addForces sums multiple forces" $ do
    let combined = f1 <+> f2
        wfSum    = addForce dt combined sys
        wfMul    = addForces dt [f1,f2] sys
    workflowDomain wfMul `shouldBe` workflowDomain wfSum

  it "errors on empty-force list" $ do
    evaluate (addForces dt [] sys) `shouldThrow` anyErrorCall
