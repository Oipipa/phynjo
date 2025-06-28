{-# LANGUAGE OverloadedStrings #-}
module ProcessSpec (spec) where

import Test.Hspec
import Action      (Action(..))
import Process     (Process(..), applyProcessWorld, applyProcessPhen)
import Literal     (emptyLiteral)
import Transition  (singletonPhen, epsilon)
import Components  (Component(AtomicC))
import qualified Data.Set as Set

spec :: Spec
spec = describe "Process module" $ do
  let t0   = 0
      lit0 = emptyLiteral

      -- no‐op Action
      noOp :: Action
      noOp = Action
        { aWorld = \t l -> (t+1, l)
        , aPhen  = \_ _ -> Set.empty
        }

      -- Action producing a single epsilon transition on "a"
      actA :: Action
      actA = Action
        { aWorld = \t l -> (t+1, l)
        , aPhen  = \t _ -> singletonPhen (epsilon (AtomicC "a") t)
        }

      -- Action producing a single epsilon transition on "b"
      actB :: Action
      actB = Action
        { aWorld = \t l -> (t+1, l)
        , aPhen  = \t _ -> singletonPhen (epsilon (AtomicC "b") t)
        }

      pNoOp = PAct noOp
      pA    = PAct actA
      pB    = PAct actB

  it "PAct noOp updates world like Action" $
    applyProcessWorld pNoOp t0 lit0 `shouldBe` (1, lit0)

  it "PAct actA collects its phenomenon" $
    applyProcessPhen pA t0 lit0
      `shouldBe` singletonPhen (epsilon (AtomicC "a") t0)

  it "PSeq composes worlds sequentially" $
    applyProcessWorld (PSeq pNoOp pNoOp) t0 lit0
      `shouldBe` (2, lit0)

  it "PSeq composes phenomena sequentially" $
    applyProcessPhen (PSeq pA pA) t0 lit0
      `shouldBe`
      Set.union
        (singletonPhen (epsilon (AtomicC "a") t0))
        (singletonPhen (epsilon (AtomicC "a") 1))

  it "PPar takes max tick and unions literals" $ do
    let (t', _) = applyProcessWorld (PPar pNoOp pNoOp) t0 lit0
    t' `shouldBe` 1

  it "PPar merges phenomena disjointly" $
    applyProcessPhen (PPar pA pNoOp) t0 lit0
      `shouldBe` singletonPhen (epsilon (AtomicC "a") t0)

  it "Parallel independent actions both appear in the phenomenon" $
    applyProcessPhen (PPar pA pB) t0 lit0
      `shouldBe`
      Set.union
        (singletonPhen (epsilon (AtomicC "a") t0))
        (singletonPhen (epsilon (AtomicC "b") t0))

  it "Nested PSeq and PPar works correctly" $
    let proc = PSeq (PPar pA pB) pNoOp
    in applyProcessPhen proc t0 lit0
         `shouldBe`
         Set.union
           (singletonPhen (epsilon (AtomicC "a") t0))
           (singletonPhen (epsilon (AtomicC "b") t0))
