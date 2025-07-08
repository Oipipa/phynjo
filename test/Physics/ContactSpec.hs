module Physics.ContactSpec (spec) where

import           Test.Hspec
import qualified Data.Map.Strict           as M

import           Physics.Contact
import           Physics.RigidState
import           Physics.Rigid3DNR         (RRune, applyRRuneWorld)
import           Components                (Component (..))

ball :: Component
ball = AtomicC "ball"

mBall, rBall :: Double
mBall = 1.0            -- kg (easy numbers)
rBall = 0.1            -- m  (10 cm)

type InertiaTensor = ( (Double,Double,Double)
                     , (Double,Double,Double)
                     , (Double,Double,Double) )

iUnit :: InertiaTensor
iUnit = ((1,0,0),(0,1,0),(0,0,1))

specList :: [(Component, Double, Double, InertiaTensor)]
specList = [(ball, rBall, mBall, iUnit)]

eConst, μConst :: Double -> Double
eConst _ = 1.0           -- perfectly elastic
μConst _ = 0.0           -- friction-less

groundRune :: RRune
groundRune = contactGroundF eConst μConst specList

runStep :: RigidState -> RigidState
runStep = applyRRuneWorld groundRune 0   -- dt ignored by solver

mkState
  :: (Double,Double,Double)  -- ^ linear v
  -> (Double,Double,Double)  -- ^ angular ω
  -> Double                  -- ^ centre y-coordinate
  -> RigidState
mkState v w y =
  emptyRigid
    { rsPos    = M.singleton ball (0,y,0)
    , rsVel    = M.singleton ball v
    , rsAngVel = M.singleton ball w
    }

spec :: Spec
spec = describe "Physics.Contact.Flexible.contactGroundF" $ do

  it "leaves state untouched when the ball is above the plane" $ do
    let st0 = mkState (0,0,0) (0,0,0) (rBall + 0.05)
        st1 = runStep st0
    st1 `shouldBe` st0

  it "pushes the ball upward if it is penetrating" $ do
    let yPen = rBall - 0.05         -- 5 cm penetration
        st0  = mkState (0,0,0) (0,0,0) yPen
        st1  = runStep st0
        (_,y0,_) = rsPos st0 M.! ball
        (_,y1,_) = rsPos st1 M.! ball
    y1 `shouldSatisfy` (> y0)

  it "reverses downward velocity for a perfectly elastic bounce" $ do
    -- Penetrate 2 cm so depth (0.02 m) > solver slop (0.01 m)
    let yDeep = rBall - 0.02
        st0   = mkState (0,-1,0) (0,0,0) yDeep
        st1   = runStep st0
        (_,vy1,_) = rsVel st1 M.! ball
    vy1 `shouldSatisfy` (> 0)