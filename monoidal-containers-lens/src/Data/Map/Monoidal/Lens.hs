module Data.Map.Monoidal.Lens () where

import Control.Lens
import Data.Map.Monoidal
import qualified Data.Map as M

type instance Index (MonoidalMap k a) = k
type instance IxValue (MonoidalMap k a) = a
instance Ord k => Ixed (MonoidalMap k a) where
    ix k f (MonoidalMap m) = case M.lookup k m of
      Just v  -> f v <&> \v' -> MonoidalMap (M.insert k v' m)
      Nothing -> pure (MonoidalMap m)
    {-# INLINE ix #-}

instance Ord k => At (MonoidalMap k a) where
    at k f (MonoidalMap m) = f mv <&> \r -> case r of
      Nothing -> maybe (MonoidalMap m) (const (MonoidalMap $ M.delete k m)) mv
      Just v' -> MonoidalMap $ M.insert k v' m
      where mv = M.lookup k m
    {-# INLINE at #-}

instance Each (MonoidalMap k a) (MonoidalMap k b) a b

instance FunctorWithIndex k (MonoidalMap k)
instance FoldableWithIndex k (MonoidalMap k)
instance TraversableWithIndex k (MonoidalMap k) where
    itraverse f (MonoidalMap m) = fmap MonoidalMap $ itraverse f m
    {-# INLINE itraverse #-}

instance Ord k => TraverseMin k (MonoidalMap k) where
    traverseMin f (MonoidalMap m) = fmap MonoidalMap $ traverseMin f m
    {-# INLINE traverseMin #-}
instance Ord k => TraverseMax k (MonoidalMap k) where
    traverseMax f (MonoidalMap m) = fmap MonoidalMap $ traverseMax f m
    {-# INLINE traverseMax #-}

instance AsEmpty (MonoidalMap k a) where
    _Empty = nearly (MonoidalMap M.empty) (M.null . getMonoidalMap)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalMap k a) where
    type Unwrapped (MonoidalMap k a) = M.Map k a
    _Wrapped' = iso getMonoidalMap MonoidalMap
    {-# INLINE _Wrapped' #-}
