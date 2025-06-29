{-# LANGUAGE OverloadedStrings #-}

module System.SystemForcesSpec (spec) where

import Test.Hspec
import Control.Exception            (evaluate)
import qualified Data.Map.Strict       as M
import Numeric.Units.Dimensional.Prelude  (( *~), kilo, gram)
import UnitLiteral                  (getLiteral)
import Body                         (mkBody)
import Components                   (Component(AtomicC))
import System.SystemForces          (System(..), mkSystem)

spec :: Spec
spec = describe "System.SystemForces.mkSystem" $ do

  let b1   = mkBody "a" (1 *~ kilo gram) 0
      b2   = mkBody "b" (2 *~ kilo gram) 5
      sys  = mkSystem [b1, b2]
      a    = AtomicC "a"
      b    = AtomicC "b"

  it "errors on empty body list" $ do
    evaluate (mkSystem []) `shouldThrow` anyErrorCall

  it "builds sMassMap with correct mass values in kg" $ do
    M.lookup a (sMassMap sys) `shouldBe` Just 1.0
    M.lookup b (sMassMap sys) `shouldBe` Just 2.0

  it "sMassMap has exactly two entries" $ do
    M.size (sMassMap sys) `shouldBe` 2

  it "preserves the original unit-checked fields" $ do
    let baseMasses = getLiteral (sMass sys)
    M.keys baseMasses `shouldMatchList` [a,b]
    let basePos = getLiteral (sPos sys)
        baseMom = getLiteral (sMom sys)
    M.keys basePos `shouldMatchList` [a,b]
    M.keys baseMom `shouldMatchList` [a,b]
