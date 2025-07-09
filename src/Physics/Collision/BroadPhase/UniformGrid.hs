{-# LANGUAGE Safe #-}

module Physics.Collision.BroadPhase.UniformGrid
  ( UniformGrid
  , initGrid
  , updateGrid
  , queryNeighbors
  , allPairsGrid
  ) where

import Physics.Collision.Types     (ComponentBB, BoundingVolume(..), AABB(..))
import Physics.Collision.BoundingVolume (intersectsAABB)
import Physics.Math.Vec3Util      (Vec3)
import Components                  (Component)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import Data.List                    (foldl')

-- | Integer cell coordinate
type CellCoord = (Int, Int, Int)

data UniformGrid = UniformGrid
  { ugCellSize  :: Double
  , ugCompCells :: M.Map Component [CellCoord]
  , ugCellMap   :: M.Map CellCoord [Component]
  , ugAABBs     :: M.Map Component AABB
  }

initGrid :: Double -> [ComponentBB] -> UniformGrid
initGrid = buildGrid

updateGrid :: UniformGrid -> [ComponentBB] -> UniformGrid
updateGrid ug bbs = buildGrid (ugCellSize ug) bbs

buildGrid :: Double -> [ComponentBB] -> UniformGrid
buildGrid cellSize bbs =
  let
    initAcc :: ( M.Map Component [CellCoord]
               , M.Map CellCoord   [Component]
               , M.Map Component   AABB
               )
    initAcc = (M.empty, M.empty, M.empty)

    insertComp
      :: ( M.Map Component [CellCoord]
         , M.Map CellCoord   [Component]
         , M.Map Component   AABB
         )
      -> ComponentBB
      -> ( M.Map Component [CellCoord]
         , M.Map CellCoord   [Component]
         , M.Map Component   AABB
         )
    insertComp (ccMap, cellMap, aabbMap) (c, BB_AABB aabb@(AABB mn mx)) =
      let
        cells    = cellsForAABB cellSize mn mx
        ccMap'   = M.insert c cells ccMap
        cellMap' = foldl' (\m cell -> M.insertWith (++) cell [c] m)
                         cellMap cells
        aabbMap' = M.insert c aabb aabbMap
      in
        (ccMap', cellMap', aabbMap')
    insertComp acc _ = acc

    (compCells, cellMap, aabbMap) = foldl' insertComp initAcc bbs
  in
    UniformGrid cellSize compCells cellMap aabbMap

cellsForAABB :: Double -> Vec3 -> Vec3 -> [CellCoord]
cellsForAABB cellSize (minX,minY,minZ) (maxX,maxY,maxZ) =
  [ (ix,iy,iz)
  | ix <- [floor (minX / cellSize) .. floor (maxX / cellSize)]
  , iy <- [floor (minY / cellSize) .. floor (maxY / cellSize)]
  , iz <- [floor (minZ / cellSize) .. floor (maxZ / cellSize)]
  ]

queryNeighbors :: UniformGrid -> Component -> [Component]
queryNeighbors (UniformGrid _ compCells cellMap aabbMap) c =
  case M.lookup c compCells of
    Nothing    -> []
    Just cells ->
      let
        cand = concatMap (\cell -> M.findWithDefault [] cell cellMap) cells
        unique = S.toList $ S.delete c $ S.fromList cand
        aabb1 = aabbMap M.! c
      in filter (\d -> let aabb2 = aabbMap M.! d in intersectsAABB aabb1 aabb2) unique

allPairsGrid :: UniformGrid -> S.Set (Component, Component)
allPairsGrid ug@(UniformGrid _ compCells _ _) =
  let pairs = do
        c <- M.keys (ugCompCells ug)
        d <- queryNeighbors ug c
        return $ orderPair c d
  in S.fromList pairs

orderPair :: Component -> Component -> (Component, Component)
orderPair a b
  | a <= b    = (a,b)
  | otherwise = (b,a)
