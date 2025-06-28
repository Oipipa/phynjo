module Main where

import Components (Component(..))
import Data.Map   (Map)
import qualified Data.Map as M

type Vec     = (Double,Double)
type MassMap = Map Component Double
type PosMap  = Map Component Vec
type MomMap  = Map Component Vec

add   (x1,y1) (x2,y2) = (x1+x2, y1+y2)
sub   (x1,y1) (x2,y2) = (x1-x2, y1-y2)
scale s (x,y)        = (s*x,   s*y)
norm  (x,y)          = sqrt (x*x + y*y)

-- compute gravitational force on body i
force :: Double -> MassMap -> PosMap -> Component -> Vec
force g mm pm i =
  let mi     = mm M.! i
      others = filter (/= i) (M.keys pm)
  in foldr (\j acc ->
       let mj = mm M.! j
           qi = pm M.! i
           qj = pm M.! j
           r  = sub qj qi
           d  = norm r
           f  = scale (g*mi*mj/(d**3)) r
       in add acc f
     ) (0,0) others

-- one Euler (kick-drift) step
eulerStep :: Double -> Double -> MassMap -> PosMap -> MomMap -> (PosMap,MomMap)
eulerStep dt g mm qm pm =
  let fs  = M.fromList [(i, force g mm qm i) | i <- M.keys qm]
      pm' = M.mapWithKey (\i pi -> add pi (scale dt (fs M.! i))) pm
      qm' = M.mapWithKey (\i qi -> add qi (scale (dt/(mm M.! i)) (pm' M.! i))) qm
  in (qm', pm')

-- one Leapfrog (drift-kick-drift) step
leapfrogStep :: Double -> Double -> MassMap -> PosMap -> MomMap -> (PosMap,MomMap)
leapfrogStep dt g mm qm pm =
  let half = dt/2
      qmMid = M.mapWithKey (\i qi -> add qi (scale (half/(mm M.! i)) (pm M.! i))) qm
      fs    = M.fromList [(i, force g mm qmMid i) | i <- M.keys qm]
      pm'   = M.mapWithKey (\i pi -> add pi (scale dt (fs M.! i))) pm
      qm'   = M.mapWithKey (\i _  -> 
                let qMid = qmMid M.! i
                    pnew = pm'  M.! i
                in add qMid (scale (half/(mm M.! i)) pnew)
              ) qmMid
  in (qm', pm')

showMap :: Map Component Vec -> String
showMap m = 
  "{" ++ concatMap (\(i,v)->show i++"="++show v++", ") (M.toList m) ++ "}"

main :: IO ()
main = do
  let c1 = AtomicC "1"
      c2 = AtomicC "2"
      c3 = AtomicC "3"
      mm = M.fromList
        [ (c1,1.0), (c2,2.0), (c3,1.0) ]
      qm = M.fromList
        [ (c1,(-1.0,0)), (c2,(0,0)), (c3,(2.0,0)) ]
      pm = M.fromList
        [ (c1,(0,0)), (c2,(0,0)), (c3,(0,0)) ]
      g  = 1.0
      dt = 0.5

      (qe, pe) = eulerStep     dt g mm qm pm
      (ql, pl) = leapfrogStep dt g mm qm pm

  putStrLn $ "Euler     q=" ++ showMap qe ++ " p=" ++ showMap pe
  putStrLn $ "Leapfrog  q=" ++ showMap ql ++ " p=" ++ showMap pl
