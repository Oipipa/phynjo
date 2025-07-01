{-# LANGUAGE OverloadedStrings #-}
module ConstraintSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

import Literal               (emptyLiteral, getLiteral, literalFromList)
import Transition            (singletonPhen, epsilon)
import Components            (Component(AtomicC))
import EventRule             (EventRule(..))
import EventWorkflow         
  ( EventWorkflow(..)
  , applyEventWorkflowWorld
  )
import Constraint            (Invariant, preservesInvariant)

spec :: Spec
spec = describe "Constraint module (over EventWorkflow)" $ do

  let t0 = 0
      a  = AtomicC "a"

      -- no‐op rule: touches nothing, no state change, emits no events
      noOpRule :: EventRule
      noOpRule = EventRule
        { erDomain = Set.empty
        , erStep   = id
        , erEvents = \_ _ -> Set.empty
        }
      noOpWF = ERun noOpRule

      -- toggles presence of 'a', but emits no events
      toggleRule :: EventRule
      toggleRule = EventRule
        { erDomain = Set.singleton a
        , erStep   = \lit ->
            let s = getLiteral lit
            in if Set.member a s
                 then literalFromList []
                 else literalFromList [a]
        , erEvents = \_ _ -> Set.empty
        }
      toggleWF = ERun toggleRule

      alwaysTrue :: Invariant
      alwaysTrue _ = True

      onlyEmpty :: Invariant
      onlyEmpty lit = getLiteral lit == Set.empty

  it "alwaysTrue is preserved by any workflow" $ do
    preservesInvariant noOpWF t0 emptyLiteral alwaysTrue
      `shouldBe` True
    preservesInvariant toggleWF t0 emptyLiteral alwaysTrue
      `shouldBe` True

  it "fails if invariant false initially" $ do
    let neverTrue :: Invariant
        neverTrue _ = False
    preservesInvariant noOpWF t0 emptyLiteral neverTrue
      `shouldBe` False

  it "fails if invariant false after toggle" $ do
    -- initially empty ⇒ onlyEmpty holds, but toggleWF breaks it
    preservesInvariant toggleWF t0 emptyLiteral onlyEmpty
      `shouldBe` False
