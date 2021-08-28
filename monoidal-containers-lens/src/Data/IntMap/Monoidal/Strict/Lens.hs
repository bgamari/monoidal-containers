module Data.IntMap.Monoidal.Strict.Lens () where

import Control.Lens
import Data.IntMap.Monoidal.Strict
import qualified Data.IntMap.Strict as M

type instance Index (MonoidalIntMap a) = Int
type instance IxValue (MonoidalIntMap a) = a
instance Ixed (MonoidalIntMap a) where
    ix k f (MonoidalIntMap m) = case M.lookup k m of
      Just v  -> f v <&> \v' -> MonoidalIntMap (M.insert k v' m)
      Nothing -> pure (MonoidalIntMap m)
    {-# INLINE ix #-}

instance At (MonoidalIntMap a) where
    at k f (MonoidalIntMap m) = f mv <&> \r -> case r of
      Nothing -> maybe (MonoidalIntMap m) (const (MonoidalIntMap $ M.delete k m)) mv
      Just v' -> MonoidalIntMap $ M.insert k v' m
      where mv = M.lookup k m
    {-# INLINE at #-}

instance Each (MonoidalIntMap a) (MonoidalIntMap b) a b

instance FunctorWithIndex Int MonoidalIntMap 
instance FoldableWithIndex Int MonoidalIntMap
instance TraversableWithIndex Int MonoidalIntMap where
    itraverse f (MonoidalIntMap m) = fmap MonoidalIntMap $ itraverse f m
    {-# INLINE itraverse #-}

instance TraverseMin Int MonoidalIntMap  where
    traverseMin f (MonoidalIntMap m) = fmap MonoidalIntMap $ traverseMin f m
    {-# INLINE traverseMin #-}
instance TraverseMax Int MonoidalIntMap where
    traverseMax f (MonoidalIntMap m) = fmap MonoidalIntMap $ traverseMax f m
    {-# INLINE traverseMax #-}

instance AsEmpty (MonoidalIntMap a) where
    _Empty = nearly (MonoidalIntMap M.empty) (M.null . getMonoidalIntMap)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalIntMap a) where
    type Unwrapped (MonoidalIntMap a) = M.IntMap a
    _Wrapped' = iso getMonoidalIntMap MonoidalIntMap
    {-# INLINE _Wrapped' #-}
