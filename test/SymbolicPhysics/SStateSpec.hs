{-# LANGUAGE OverloadedStrings #-}

module SymbolicPhysics.SStateSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import Components (Component(..))
import SymbolicPhysics.SymbolicD
import SymbolicPhysics.SState

spec :: Spec
spec = describe "SymbolicPhysics.SState" $ do

  let a = AtomicC "a"
      b = AtomicC "b"
      exprX = var "x"
      exprY = var "y"
      state0 = emptySState
      state1 = insertS a exprX $ insertS b exprY state0

  describe "insertS and lookupS" $ do
    it "inserts and looks up values" $
      lookupS a state1 `shouldBe` exprX

    it "returns Const 0 for missing key" $
      lookupS (AtomicC "nope") state1 `shouldBe` constant 0

    it "can override an existing key" $
      let state2 = insertS a (constant 99) state1
      in lookupS a state2 `shouldBe` constant 99

  describe "emptySState" $ do
    it "is empty" $
      keysS emptySState `shouldBe` []

  describe "fromListS" $ do
    it "builds a state from a list" $
      let st = fromListS [(a, var "foo"), (b, var "bar")]
      in lookupS a st `shouldBe` var "foo"

  describe "keysS" $ do
    it "returns the correct list of keys" $
      keysS state1 `shouldMatchList` [a,b]
