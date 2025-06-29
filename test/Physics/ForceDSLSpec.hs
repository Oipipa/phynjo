{-# LANGUAGE OverloadedStrings #-}

module Physics.ForceDSLSpec (spec) where

import Test.Hspec
import Control.Exception       (evaluate)

import Physics.ForceDSL        (addForce, addForces)
import Physics.Force           (Force(..), (<+>))
import NSpell                   (domainS)
import Components               (Component(AtomicC))
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
    let spell = addForce dt f1 sys
    domainS spell `shouldBe` S.fromList [a,b]

  it "addForces on one force equals addForce" $ do
    let s1 = addForces dt [f1] sys
        s2 = addForce  dt  f1  sys
    domainS s1 `shouldBe` domainS s2

  it "addForces sums multiple forces" $ do
    let combined = f1 <+> f2
        sSum     = addForce dt combined sys
        sMul     = addForces dt [f1,f2] sys
    domainS sMul `shouldBe` domainS sSum

  it "errors on empty-force list" $ do
    evaluate (addForces dt [] sys) `shouldThrow` anyErrorCall
