{-# LANGUAGE NamedFieldPuns #-}
module Physics.DriftNR
  ( driftNR        -- :: [(Component,Double)] -> NRune
  ) where

import Components        (Component)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import NRune
import NState
import ScalarLiteral

driftNR :: [(Component,Double)] -> NRune
driftNR masses =
  let dom       = S.fromList (map fst masses)
      massMap   = M.fromList masses           -- Component ↦ m_i
      step dt st =
        foldr
          (\c acc ->
              let m   = massMap M.! c
                  qi  = lookupPos c acc
                  pi  = lookupMom c acc
                  qi' = qi + dt * pi / m
              in insertPos c qi' acc)
          st
          (map fst masses)
  in NR { domainN = dom, stepN = step }
