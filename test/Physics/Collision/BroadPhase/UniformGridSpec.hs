{-# LANGUAGE OverloadedStrings #-}

module Physics.Collision.BroadPhase.UniformGridSpec (spec) where

import Test.Hspec
import Physics.Collision.BroadPhase.UniformGrid
import Physics.Collision.Types            (AABB(..), BoundingVolume(..))
import Components                         (Component(AtomicC))
import qualified Data.Set           as S

spec :: Spec
spec = describe "Physics.Collision.BroadPhase.UniformGrid" $ do
  let c1   = AtomicC "a"
      c2   = AtomicC "b"
      c3   = AtomicC "c"
      a1   = AABB (0,0,0)   (1,1,1)
      a2   = AABB (1,1,1)   (2,2,2)
      a3   = AABB (2.1,0,0) (3.1,1,1)
      bb1  = (c1, BB_AABB a1)
      bb2  = (c2, BB_AABB a2)
      bb3  = (c3, BB_AABB a3)
      cellSize = 1.0

      grid12 = initGrid cellSize [bb1, bb2]
      grid123 = initGrid cellSize [bb1, bb2, bb3]

  it "neighbors for overlapping cells" $ do
    queryNeighbors grid12 c1 `shouldBe` [c2]
    queryNeighbors grid12 c2 `shouldBe` [c1]

  it "no neighbors for separate cells" $ do
    queryNeighbors grid123 c1 `shouldBe` [c2]
    queryNeighbors grid123 c3 `shouldBe` []

  it "allPairsGrid reports exactly one pair for two neighbors" $ do
    allPairsGrid grid12 `shouldBe` S.singleton (c1,c2)

  it "allPairsGrid ignores non-overlaps" $ do
    allPairsGrid grid123 `shouldBe` S.singleton (c1,c2)

  it "updateGrid updates cell associations" $ do
    let moved2 = (c2, BB_AABB (AABB (0.5,0.5,0.5) (1.5,1.5,1.5)))
        gridMoved = updateGrid grid12 [bb1, moved2]
    queryNeighbors gridMoved c1 `shouldBe` [c2]
    allPairsGrid gridMoved `shouldBe` S.singleton (c1,c2)
