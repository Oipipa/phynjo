{-# LANGUAGE OverloadedStrings #-}

module Physics.Collision.NarrowPhaseSpec (spec) where

import Test.Hspec
import Physics.Collision.NarrowPhase (narrowPhase, Shape(..))
import Physics.RigidBodyUtilities.Rigid3DNR            (RRune(..))
import Components                   (Component(AtomicC))
import Data.Set                     (Set)
import qualified Data.Set as S

spec :: Spec
spec = describe "Physics.Collision.NarrowPhase" $ do
  let a            = AtomicC "a"
      b            = AtomicC "b"
      restitution  = const 1.0
      friction     = const 0.0
      iterations   = 1
      shapes       = [(a, ShSphere 1.0), (b, ShSphere 1.0)]
      pairsOverlap = S.singleton (a,b)
      pairsEmpty   = S.empty
      ruleOverlap  = narrowPhase restitution friction iterations shapes pairsOverlap
      ruleEmpty    = narrowPhase restitution friction iterations shapes pairsEmpty

  it "domain contains both a and b for overlapping spheres" $
    domainR ruleOverlap `shouldBe` S.fromList [a,b]

  it "domain empty when no candidate pairs" $
    domainR ruleEmpty `shouldBe` S.empty

  it "ignores non-sphere shapes" $
    let shapes' = [(a, ShPlane), (b, ShSphere 1.0)]
        rule2   = narrowPhase restitution friction iterations shapes' (S.singleton (a,b))
    in domainR rule2 `shouldBe` S.fromList [b]
