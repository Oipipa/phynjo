{-# LANGUAGE OverloadedStrings #-}

module Physics.ContactSpec (spec) where

import Test.Hspec
import Components           ( Component(AtomicC) )
import Physics.Contact      ( contactGround, contactSpheres )
import Physics.Rigid3DNR    ( applyRRuneWorld )
import Physics.RigidState   ( emptyRigid
                            , insertRigid
                            , lookupVelR
                            , lookupPosR
                            )
import Physics.LeapfrogNR   ( Vec3 )
import qualified Data.Map.Strict as M

spec :: Spec
spec = describe "Physics.Contact" $ do

  let dt = 0.1
      c  = AtomicC "c"

  describe "contactGround" $ do
    let rune = contactGround 0.8 [c]

    it "does nothing if above ground or moving upward" $ do
      -- y = 1, vy = +1 → no bounce
      let st = insertRigid c (0,1,0) (1,0,0,0) (0,1,0) (0,0,0) emptyRigid
      applyRRuneWorld rune dt st `shouldBe` st

    it "inverts and scales vy when below ground moving down" $ do
      -- y = -0.5, vy = -2 → vy' = -e * (-2) = 1.6
      let st0 = insertRigid c (0,-0.5,0) (1,0,0,0) (0,-2,0) (0,0,0) emptyRigid
          st1 = applyRRuneWorld rune dt st0
          (vx,vy,vz) = lookupVelR c st1
      vx  `shouldBe` 0
      vz  `shouldBe` 0
      vy  `shouldBe` 1.6

  describe "contactSpheres" $ do
    let a = AtomicC "a"
        b = AtomicC "b"
        specs = [ (a,1.0,1.0)
                , (b,1.0,1.0)
                ]
        rune = contactSpheres 1.0 specs  -- perfectly elastic

    it "swaps velocities in an elastic head-on collision" $ do
      -- place them overlapping by .1: dist=.9<2
      -- a moving right at 1, b stationary → after collision, a stops, b moves right 1
      let st0 = insertRigid a (-0.5,0,0) (1,0,0,0) (1,0,0) (0,0,0) $
                insertRigid b ( 0.4,0,0) (1,0,0,0) (0,0,0) (0,0,0) emptyRigid
          st1 = applyRRuneWorld rune dt st0
      lookupVelR a st1 `shouldBe` (0,0,0)
      lookupVelR b st1 `shouldBe` (1,0,0)

    it "does nothing when spheres do not overlap" $ do
      let stNo = insertRigid a (-5,0,0) (1,0,0,0) (1,0,0) (0,0,0) $
                 insertRigid b ( 5,0,0) (1,0,0,0) (0,0,0) (0,0,0) emptyRigid
      applyRRuneWorld rune dt stNo `shouldBe` stNo
