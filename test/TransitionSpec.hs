{-# LANGUAGE OverloadedStrings #-}
module TransitionSpec (spec) where

import Test.Hspec
import Components (Component(AtomicC, Composite))
import BooleanUtils.Transition
import qualified Data.Set as Set

spec :: Spec
spec = describe "Transition module" $ do

  let a    = AtomicC "a"
      b    = AtomicC "b"
      c    = AtomicC "c"
      t0   = 5
      tr   = Transition a b t0 (t0+1)
      ep   = epsilon a t0

  it "epsilon c t yields src=c, tgt=c, tto=t+1" $ do
    src ep   `shouldBe` a
    tgt ep   `shouldBe` a
    tfrom ep `shouldBe` t0
    tto ep   `shouldBe` t0 + 1

  it "domain of non-epsilon transition includes both src and tgt" $ do
    domain tr `shouldBe` Set.fromList [a,b]

  it "domain of epsilon transition is empty" $ do
    domain ep `shouldBe` Set.empty

  it "emptyPhen is the empty set" $ do
    emptyPhen `shouldBe` Set.empty

  it "singletonPhen wraps a transition" $ do
    singletonPhen tr `shouldBe` Set.singleton tr

  it "unionPhen succeeds when phenomena are disjoint" $ do
    let tr2 = Transition c (AtomicC "d") t0 (t0+1)
        p1  = singletonPhen tr
        p2  = singletonPhen tr2
    unionPhen p1 p2 `shouldBe` Just (Set.fromList [tr, tr2])

  it "unionPhen fails on overlapping domains" $ do
    let trOverlap = Transition a (AtomicC "x") t0 (t0+1)
    unionPhen (singletonPhen tr) (singletonPhen trOverlap)
      `shouldBe` Nothing
