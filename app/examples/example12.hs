{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Monad          (foldM)
import           Data.List              (intercalate)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Numeric                (showFFloat)
import           System.IO              (hPutStrLn, stdout)

import           Components             (Component (..))
import           Physics.LeapfrogNR     (Vec3, vscale)
import           Physics.RigidBody      (Quaternion)
import           Physics.RigidState
import           Physics.Rigid3DNR      ( RRune (..), applyRRuneWorld
                                        , driftTrans, driftRot
                                        , kickForce3D )
import           Physics.Force3D        (gravity3D, Force3D (..) )
import qualified Physics.Extra  as F
import           Physics.Contact (contactGroundF)

ball        :: Component
ball        = AtomicC "ball"

mBall, rBall, areaBall :: Double
mBall    = 0.0027 
rBall    = 0.020 
areaBall = pi * rBall * rBall 

type InertiaTensor = (Vec3,Vec3,Vec3)
inertiaBall :: InertiaTensor
inertiaBall = let i = (2/3) * mBall * rBall * rBall
              in  ((i,0,0),(0,i,0),(0,0,i))

gEarth   = 9.81 

rhoAir = 1.225
cdBall = 0.46
clBall = 0.18

cOmega = 1.0e-8      -- N m s   (~0.9 s half-life)

eRest, muK :: Double -> Double
eRest vn = 0.87 - 0.02 * min 4 vn      -- drops slightly with impact speed
muK  _  = 0.20                         -- kinetic μ

muStatic, epsStick :: Double
muStatic = 0.35
epsStick = 0.01 

scaleForce3D :: Double -> Force3D -> Force3D
scaleForce3D k (Force3D g) =
  Force3D $ \st c -> let (f,τ) = g st c
                     in  (vscale k f, vscale k τ)


driftHalfT, driftHalfR :: RRune
driftHalfT = driftTrans [ball]
driftHalfR = driftRot   [ball]

contactGroundFixed :: RRune
contactGroundFixed =
  let spec = [(ball, rBall, mBall, inertiaBall)]
      muFun vt = if vt < epsStick then muStatic else muK vt
  in  contactGroundF eRest muFun spec

massMap  = [(ball, mBall)]
inertMap = [(ball, inertiaBall)]
fieldAll :: Force3D
fieldAll = F.sumForces3D
  [ gravity3D gEarth (M.fromList massMap)
  , F.dragQuad3D  rhoAir cdBall areaBall
  , F.magnus3D    rhoAir clBall areaBall rBall
  , F.dragTorque3D cOmega
  ]

halfKick :: RRune
halfKick = kickForce3D massMap inertMap (scaleForce3D 0.5 fieldAll)

composeRRune :: [RRune] -> RRune
composeRRune rs =
  let dom        = S.unions (map domainR rs)
      step dt st = foldl (\s r -> stepR r dt s) st rs
  in  RR dom step

fullStepRune :: RRune
fullStepRune = composeRRune
  [ halfKick
  , driftHalfT, driftHalfR
  , contactGroundFixed
  , driftHalfT, driftHalfR
  , halfKick
  ]

qIdentity :: Quaternion
qIdentity = (1,0,0,0)

st0 :: RigidState
st0 = emptyRigid
  { rsPos    = M.singleton ball (0, 0.30, 0)     -- 30 cm above table
  , rsOri    = M.singleton ball qIdentity
  , rsVel    = M.singleton ball ( 2.0, -1.0, 0)  -- forward & slight down
  , rsAngVel = M.singleton ball (0, 0, 150)      -- topspin 150 rad s⁻¹
  }

dt, simT :: Double
dt   = 0.0005       -- 0.5 ms
simT = 2.0          -- 2 s
nSteps :: Int
nSteps = round (simT / dt)

stepOnce :: RigidState -> RigidState
stepOnce = applyRRuneWorld fullStepRune dt

ff :: Double -> String
ff x = showFFloat (Just 6) x ""

vecShow :: Vec3 -> String
vecShow (x,y,z) = intercalate "," [ff x, ff y, ff z]

quatShow :: Quaternion -> String
quatShow (w,x,y,z) = intercalate "," [ff w, ff x, ff y, ff z]

dumpLine :: Double -> RigidState -> String
dumpLine t st =
  let p = rsPos    st M.! ball
      q = rsOri    st M.! ball
      v = rsVel    st M.! ball
      w = rsAngVel st M.! ball
  in  intercalate ","
        [ ff t
        , vecShow p
        , quatShow q
        , vecShow v
        , vecShow w ]

header :: String
header = "t,x,y,z,qw,qx,qy,qz,vx,vy,vz,wx,wy,wz"

main :: IO ()
main = do
  hPutStrLn stdout header
  _ <- foldM
         (\(t,st) _ -> do
             hPutStrLn stdout (dumpLine t st)
             let st' = stepOnce st
             pure (t + dt, st'))
         (0.0, st0)
         [1 .. nSteps]
  pure ()
