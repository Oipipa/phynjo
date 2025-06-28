module Main where

import Components (Component(..))
import Data.Map   (Map)
import qualified Data.Map as M

type Vec     = (Double, Double)
type MassMap = Map Component Double
type PosMap  = Map Component Vec

com :: Component -> MassMap -> PosMap -> Vec
com c mm pm = case c of
  AtomicC _ ->
    pm M.! c
  Composite cs ->
    let (totalM,(sx,sy)) =
          foldr
            (\ci (tm,(ax,ay)) ->
               let m     = mm M.! ci
                   (x,y) = com ci mm pm
               in (tm + m, (ax + m * x, ay + m * y))
            )
            (0,(0,0))
            cs
    in (sx / totalM, sy / totalM)

translate :: Vec -> PosMap -> PosMap
translate (vx,vy) = M.map (\(x,y) -> (x + vx, y + vy))

main :: IO ()
main = do
  let c    = Composite [AtomicC "c1", AtomicC "c2"]
      mm   = M.fromList [(AtomicC "c1",1.0),(AtomicC "c2",3.0)]
      pm   = M.fromList [(AtomicC "c1",(0,0)),   (AtomicC "c2",(2,0))]
      v    = (1,1)
      pm'  = translate v pm
      com0 = com c mm pm
      com1 = com c mm pm'
  print com0
  print com1
