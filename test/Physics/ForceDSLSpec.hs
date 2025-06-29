-- test/Physics/ForceDSLSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module Physics.ForceDSLSpec (spec) where

import Test.Hspec
import Control.Exception       (evaluate)

import Physics.ForceDSL        (addForce, addForces)
import Physics.Force           (Force(..))
import NSpell                   (domainS)
import Components               (Component(AtomicC))
import Body                     (mkBody)
import System.SystemForces      (mkSystem, System(..))
import Numeric.Units.Dimensional.Prelude
  ( (*~), kilo, gram )

import qualified Data.Set     as S

spec :: Spec
spec = describe "Physics.ForceDSL" $ do
  let -- one-body system “a” of 1 kg at x=0
      b1   = mkBody "a" (1 *~ kilo gram) 0
      sys  = mkSystem [b1]                    -- extended System.SystemForces.System
      a    = AtomicC "a"
      dt   = 0.1

      -- two dummy custom forces
      f1, f2 :: Force
      f1 = Custom $ \_ _ -> (1,0,0)
      f2 = Custom $ \_ _ -> (2,0,0)

  it "addForce produces a spell whose domain is exactly the body set" $ do
    let spell = addForce dt f1 sys
    domainS spell `shouldBe` S.singleton a

  it "addForces on one force reduces to addForce" $ do
    let spell1 = addForces dt [f1] sys
        spell2 = addForce  dt f1      sys
    domainS spell1 `shouldBe` domainS spell2

  it "addForces on multiple forces still has the same domain" $ do
    let spell = addForces dt [f1, f2] sys
    domainS spell `shouldBe` S.singleton a

  it "addForces errors when given an empty force list" $ do
    evaluate (addForces dt [] sys)
      `shouldThrow` anyErrorCall
