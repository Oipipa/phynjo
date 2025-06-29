module ScalarLiteral
  ( SLit(..)                -- opaque
  , emptySL
  , singletonSL
  , lookupSL
  , insertSL
  , adjustSL            -- update existing value or insert default
  , keysSL
  ) where

import Components                 (Component)
import Data.Map.Strict            (Map)
import qualified Data.Map.Strict  as M

newtype SLit = SL (Map Component Double)
  deriving (Eq, Show)

emptySL :: SLit
emptySL = SL M.empty

singletonSL :: Component -> Double -> SLit
singletonSL c x = SL (M.singleton c x)

lookupSL :: Component -> SLit -> Double   -- default 0
lookupSL c (SL m) = M.findWithDefault 0 c m

insertSL :: Component -> Double -> SLit -> SLit
insertSL c x (SL m) = SL (M.insert c x m)

adjustSL :: (Double -> Double) -> Double -> Component -> SLit -> SLit
adjustSL f def c (SL m) =
  SL (M.alter (Just . f . maybe def id) c m)

keysSL :: SLit -> [Component]
keysSL (SL m) = M.keys m
