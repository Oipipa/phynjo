{-# LANGUAGE OverloadedStrings #-}
module ActionSpec (spec) where

import Test.Hspec
import BooleanUtils.Literal    (emptyLiteral)
import BooleanUtils.Transition (Phenomenon, emptyPhen, singletonPhen, Transition(..), epsilon)
import Action     (Action(..), applyActionWorld, applyActionPhen)
import Components (Component(AtomicC))
import qualified Data.Set as Set

spec :: Spec
spec = describe "Action module" $ do

  let t0    = 0
      lit0  = emptyLiteral
      -- A no-op action: advances tick by 1, leaves literal unchanged, produces no Phenomenon
      noOp :: Action
      noOp = Action
        { aWorld = \t l -> (t + 1, l)
        , aPhen  = \_ _ -> emptyPhen
        }
      -- A single-epsilon Phenomenon action on component "a"
      actPhen :: Action
      actPhen = Action
        { aWorld = \t l -> (t + 1, l)
        , aPhen  = \t _ -> singletonPhen (epsilon (AtomicC "a") t)
        }

  it "applyActionWorld [] returns the initial tick and literal" $ do
    applyActionWorld [] t0 lit0 `shouldBe` (t0, lit0)

  it "applyActionWorld [noOp] advances tick by 1 and literal unchanged" $ do
    applyActionWorld [noOp] t0 lit0 `shouldBe` (t0 + 1, lit0)

  it "applyActionWorld [noOp,noOp] advances tick by 2" $ do
    applyActionWorld [noOp, noOp] t0 lit0 `shouldBe` (t0 + 2, lit0)

  it "applyActionPhen [] yields empty Phenomenon" $ do
    applyActionPhen [] t0 lit0 `shouldBe` Set.empty

  it "applyActionPhen [noOp] yields empty Phenomenon" $ do
    applyActionPhen [noOp] t0 lit0 `shouldBe` Set.empty

  it "applyActionPhen [actPhen] yields the singleton epsilon Phenomenon" $ do
    applyActionPhen [actPhen] t0 lit0
      `shouldBe` singletonPhen (epsilon (AtomicC "a") t0)

  it "applyActionPhen [actPhen, actPhen] collects both Phenomena" $ do
    let p1 = singletonPhen (epsilon (AtomicC "a") t0)
        p2 = singletonPhen (epsilon (AtomicC "a") (t0+1))
    applyActionPhen [actPhen, actPhen] t0 lit0
      `shouldBe` Set.union p1 p2
