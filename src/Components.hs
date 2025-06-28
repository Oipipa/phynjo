module Components
  ( Component(..)
  , children
  , arity
  , subcomponents
  ) where

-- | A Component is either atomic (identified by a String)
-- or a composite built from an ordered list of subcomponents.
data Component
  = AtomicC String
  | Composite [Component]
  deriving (Eq, Ord, Show)

-- | Immediate children of a component
children :: Component -> [Component]
children (AtomicC _)      = []
children (Composite subs) = subs

-- | Arity = number of immediate children
arity :: Component -> Int
arity = length . children

-- | All subcomponents (including itself)
subcomponents :: Component -> [Component]
subcomponents c@(AtomicC _)    = [c]
subcomponents c@(Composite cs) = c : concatMap subcomponents cs
