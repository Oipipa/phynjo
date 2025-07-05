{-# LANGUAGE RecordWildCards #-}

module Physics.ContactSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import Physics.Contact
import Physics.Rigid3DNR      (RRune(..))
import Physics.RigidState     (RigidState(..))
import Physics.LeapfrogNR     (Vec3)
import Components             (Component(..))

-- | A simple Quaternion alias used only for orientation maps in tests.
--   Change this if your actual Quaternion type differs.
type Quaternion = (Double, Double, Double, Double)

identityQ :: Quaternion
identityQ = (1,0,0,0)

spec :: Spec
spec = do
  ---------------------------------------------------------------------------
  -- Ground‑sphere tests
  ---------------------------------------------------------------------------
  let comp1   = AtomicC "c1"
      unitI   = ((1,0,0),(0,1,0),(0,0,1))
      specs1  = [(comp1, 1.0, 1.0, unitI)]
      rrG     = contactGround 1.0 0.0 specs1

      initPos = M.singleton comp1 (0, 2, 0) :: M.Map Component Vec3
      initVel = M.singleton comp1 (0, 0, 0) :: M.Map Component Vec3
      initAng = M.singleton comp1 (0, 0, 0) :: M.Map Component Vec3
      initOri = M.singleton comp1 identityQ  :: M.Map Component Quaternion

      initSt  = RigidState
                  { rsPos    = initPos
                  , rsOri    = initOri
                  , rsVel    = initVel
                  , rsAngVel = initAng
                  }

  describe "contactGround" $ do
    it "does nothing when sphere is above ground" $ do
      stepR rrG 0.1 initSt `shouldBe` initSt

    context "when sphere is penetrating the ground" $ do
      let r      = 1.0
          y0     = 0.5
          slop   = 0.01
          beta   = 0.2
          depth  = max 0 (r - y0 - slop)
          yExp   = y0 + beta * depth
          state2 = initSt { rsPos = M.singleton comp1 (0, y0, 0) }
          out2   = stepR rrG 0.1 state2
      it "corrects position by Baumgarte term" $ do
        let (_, y1, _) = rsPos out2 M.! comp1
        y1 `shouldBe` yExp

  ---------------------------------------------------------------------------
  -- Sphere‑sphere tests
  ---------------------------------------------------------------------------
  describe "contactSpheres" $ do
    let comp2   = AtomicC "c2"
        specs2  = [ (comp1, 1.0, 1.0, unitI)
                  , (comp2, 1.0, 1.0, unitI)
                  ]
        rrS     = contactSpheres 1.0 0.0 1 specs2

        posA    = M.fromList [ (comp1, (-5, 0, 0))
                              , (comp2, ( 5, 0, 0))
                              ]
        velA    = M.fromList [ (comp1, (0, 0, 0))
                              , (comp2, (0, 0, 0))
                              ]
        angA    = M.fromList [ (comp1, (0, 0, 0))
                              , (comp2, (0, 0, 0))
                              ]
        oriA    = M.fromList [ (comp1, identityQ)
                              , (comp2, identityQ)
                              ]
        initS2  = RigidState
                   { rsPos    = posA
                   , rsOri    = oriA
                   , rsVel    = velA
                   , rsAngVel = angA
                   }

    it "does nothing when spheres are far apart" $ do
      stepR rrS 0.1 initS2 `shouldBe` initS2

    context "when spheres overlap" $ do
      let r       = 1.0
          overlap = 0.5
          x1      = -(r - overlap/2)
          x2      =  (r - overlap/2)
          slop    = 0.01
          beta    = 0.2
          depth   = max 0 ((2*r) - (x2 - x1) - slop)
          corr    = beta * depth / 2

          posB    = M.fromList [ (comp1, (x1, 0, 0))
                               , (comp2, (x2, 0, 0))
                               ]
          stateB  = initS2 { rsPos = posB }
          outB    = stepR rrS 0.1 stateB
      it "applies positional correction to both spheres" $ do
        let (x1', _, _) = rsPos outB M.! comp1
            (x2', _, _) = rsPos outB M.! comp2
        x1' `shouldBe` (x1 - corr)
        x2' `shouldBe` (x2 + corr)
