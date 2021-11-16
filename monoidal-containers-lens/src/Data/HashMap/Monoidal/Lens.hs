module Data.HashMap.Monoidal.Lens () where

import Control.Lens
import Data.HashMap.Monoidal
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)

type instance Index (MonoidalHashMap k a) = k
type instance IxValue (MonoidalHashMap k a) = a
instance (Eq k, Hashable k) => Ixed (MonoidalHashMap k a) where
    ix k f (MonoidalHashMap m) = case M.lookup k m of
      Just v  -> f v <&> \v' -> MonoidalHashMap (M.insert k v' m)
      Nothing -> pure (MonoidalHashMap m)
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (MonoidalHashMap k a) where
    at k f (MonoidalHashMap m) = f mv <&> \r -> case r of
      Nothing -> maybe (MonoidalHashMap m) (const (MonoidalHashMap $ M.delete k m)) mv
      Just v' -> MonoidalHashMap $ M.insert k v' m
      where mv = M.lookup k m
    {-# INLINE at #-}

instance Each (MonoidalHashMap k a) (MonoidalHashMap k b) a b

instance (Eq k, Hashable k) => FunctorWithIndex k (MonoidalHashMap k)
instance (Eq k, Hashable k) => FoldableWithIndex k (MonoidalHashMap k)
instance (Eq k, Hashable k) => TraversableWithIndex k (MonoidalHashMap k) where
    itraverse f (MonoidalHashMap m) = fmap MonoidalHashMap $ itraverse f m
    {-# INLINE itraverse #-}

instance AsEmpty (MonoidalHashMap k a) where
    _Empty = nearly (MonoidalHashMap M.empty) (M.null . getMonoidalHashMap)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalHashMap k a) where
    type Unwrapped (MonoidalHashMap k a) = M.HashMap k a
    _Wrapped' = iso getMonoidalHashMap MonoidalHashMap
    {-# INLINE _Wrapped' #-}
