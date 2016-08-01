{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides a 'Data.Map' variant which uses the value's
-- 'Monoid' instance to accumulate conflicting entries when merging
-- 'Map's.
--
-- While some functions mirroring those of 'Data.Map' are provided
-- here for convenience, more specialized needs will likely want to use
-- either the 'Newtype' or 'Wrapped' instances to manipulate the
-- underlying 'Map'.

module Data.Map.Monoidal
    ( MonoidalMap
      -- * Often-needed functions
    , singleton
    , size
    , member
    , notMember
    , findWithDefault
    , assocs
    , elems
    , keys
    ) where

import Data.Monoid
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative (Applicative, pure)
import Data.Data (Data)
import Data.Typeable (Typeable)

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(..))
#endif

import Control.DeepSeq
import qualified Data.Map as M
import Control.Lens
import Control.Newtype

-- | A 'Map' with monoidal accumulation
newtype MonoidalMap k a = MonoidalMap { getMonoidalMap :: M.Map k a }
    deriving (Show, Read, Functor, Eq, Ord, NFData,
              Foldable, Traversable,
              Data, Typeable)

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

instance Ord k => FunctorWithIndex k (MonoidalMap k)
instance Ord k => FoldableWithIndex k (MonoidalMap k)
instance Ord k => TraversableWithIndex k (MonoidalMap k) where
    itraverse f (MonoidalMap m) = fmap MonoidalMap $ itraverse f m
    {-# INLINE itraverse #-}

instance Ord k => TraverseMin k (MonoidalMap k) where
    traverseMin f (MonoidalMap m) = fmap MonoidalMap $ traverseMin f m
    {-# INLINE traverseMin #-}
instance Ord k => TraverseMax k (MonoidalMap k) where
    traverseMax f (MonoidalMap m) = fmap MonoidalMap $ traverseMax f m
    {-# INLINE traverseMax #-}

instance AsEmpty (MonoidalMap k a) where
    _Empty = nearly (MonoidalMap M.empty) (M.null . unpack)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalMap k a) where
    type Unwrapped (MonoidalMap k a) = M.Map k a
    _Wrapped' = iso unpack pack
    {-# INLINE _Wrapped' #-}

instance (Ord k, Monoid a) => Monoid (MonoidalMap k a) where
    mempty = MonoidalMap mempty
    {-# INLINE mempty #-}
    MonoidalMap a `mappend` MonoidalMap b = MonoidalMap $ M.unionWith mappend a b
    {-# INLINE mappend #-}

instance Newtype (MonoidalMap k a) (M.Map k a) where
    pack = MonoidalMap
    {-# INLINE pack #-}
    unpack (MonoidalMap a) = a
    {-# INLINE unpack #-}

#if MIN_VERSION_base(4,7,0)
instance (Ord k, Monoid a) => IsList (MonoidalMap k a) where
    type Item (MonoidalMap k a) = (k, a)
    fromList = MonoidalMap . M.fromListWith mappend
    {-# INLINE fromList #-}
    toList = M.toList . unpack
    {-# INLINE toList #-}
#endif

-- | /O(1)/. A map with a single element.
singleton :: Ord k => k -> a -> MonoidalMap k a
singleton k a = MonoidalMap $ M.singleton k a
{-# INLINE singleton #-}

-- | /O(1)/. The number of elements in the map.
size :: MonoidalMap k a -> Int
size = M.size . unpack
{-# INLINE size #-}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: Ord k => k -> MonoidalMap k a -> Bool
member k = M.member k . unpack
{-# INLINE member #-}

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
notMember :: Ord k => k -> MonoidalMap k a -> Bool
notMember k = not . M.member k . unpack
{-# INLINE notMember #-}

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
findWithDefault :: Ord k => a -> k -> MonoidalMap k a -> a
findWithDefault def k = M.findWithDefault def k . unpack
{-# INLINE findWithDefault #-}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: Ord k => k -> MonoidalMap k a -> MonoidalMap k a
delete k = _Wrapping' MonoidalMap %~ M.delete k
{-# INLINE delete #-}

-- | /O(n)/. Return all elements of the map and their keys
assocs :: MonoidalMap k a -> [(k,a)]
assocs = M.assocs . unpack
{-# INLINE assocs #-}

-- | /O(n)/. Return all elements of the map in the ascending order of their
-- keys. Subject to list fusion.
elems :: MonoidalMap k a -> [a]
elems = M.elems . unpack
{-# INLINE elems #-}

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
keys :: MonoidalMap k a -> [k]
keys = M.keys . unpack
{-# INLINE keys #-}
