{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides a 'Data.HashMap' variant which uses the value's
-- 'Monoid' instance to accumulate conflicting entries when merging
-- 'Map's.
--
-- While some functions mirroring those of 'Data.HashMap' are provided
-- here for convenience, more specialized needs will likely want to use
-- either the 'Newtype' or 'Wrapped' instances to manipulate the
-- underlying 'Map'.

module Data.HashMap.Monoidal
    ( MonoidalHashMap(..)
      -- * Often-needed functions
    , toList
    , fromList
    , fromListWith
    , singleton
    , size
    , member
    , notMember
    , lookup
    , lookupM
    , elems
    , keys
    , delete
    , mapKeys
    , insert
    , insertWith
    , modify
    , modifyDef
    , map
    , filterWithKey
    ) where

import Prelude hiding (lookup, map)
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Foldable (Foldable)
import Control.Applicative (pure)
import Data.Data (Data)
import Data.Typeable (Typeable)

#if MIN_VERSION_base(4,7,0)
import qualified GHC.Exts as Exts
#endif

import Control.DeepSeq
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
#if MIN_VERSION_unordered_containers(0,2,8)
import Data.Hashable.Lifted (Hashable1)
#endif
import Control.Lens
import Control.Newtype
import Data.Align

-- | A 'HashMap' with monoidal accumulation
newtype MonoidalHashMap k a = MonoidalHashMap { getMonoidalHashMap :: M.HashMap k a }
    deriving ( Show, Read, Functor, Eq, NFData
             , Foldable, Traversable, Data, Typeable, Hashable, Align
#if MIN_VERSION_unordered_containers(0,2,8)
             , Hashable1
#endif
#if MIN_VERSION_these(0,8,0)
             , Semialign
#endif
             )

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
    _Empty = nearly (MonoidalHashMap M.empty) (M.null . unpack)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalHashMap k a) where
    type Unwrapped (MonoidalHashMap k a) = M.HashMap k a
    _Wrapped' = iso unpack pack
    {-# INLINE _Wrapped' #-}

instance (Eq k, Hashable k) => Rewrapped (M.HashMap k a) (MonoidalHashMap k a)

instance (Eq k, Hashable k) => Rewrapped (MonoidalHashMap k a) (M.HashMap k a)

instance (Eq k, Hashable k, Semigroup a) => Semigroup (MonoidalHashMap k a) where
    MonoidalHashMap a <> MonoidalHashMap b = MonoidalHashMap $ M.unionWith (<>) a b
    {-# INLINE (<>) #-}

instance (Eq k, Hashable k, Semigroup a) => Monoid (MonoidalHashMap k a) where
    mempty = MonoidalHashMap mempty
    {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
    mappend (MonoidalHashMap a) (MonoidalHashMap b) = MonoidalHashMap $ M.unionWith (<>) a b
    {-# INLINE mappend #-}
#endif

instance Newtype (MonoidalHashMap k a) (M.HashMap k a) where
    pack = MonoidalHashMap
    {-# INLINE pack #-}
    unpack (MonoidalHashMap a) = a
    {-# INLINE unpack #-}

#if MIN_VERSION_base(4,7,0)
instance (Eq k, Hashable k, Semigroup a) => Exts.IsList (MonoidalHashMap k a) where
    type Item (MonoidalHashMap k a) = (k, a)
    fromList = MonoidalHashMap . M.fromList
    {-# INLINE fromList #-}
    toList = M.toList . unpack
    {-# INLINE toList #-}
#endif

-- | /O(1)/. A map with a single element.
singleton :: (Eq k, Hashable k) => k -> a -> MonoidalHashMap k a
singleton k a = MonoidalHashMap $ M.singleton k a
{-# INLINE singleton #-}

-- | /O(1)/. The number of elements in the map.
size :: MonoidalHashMap k a -> Int
size = M.size . unpack
{-# INLINE size #-}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: (Eq k, Hashable k) => k -> MonoidalHashMap k a -> Bool
member k = M.member k . unpack
{-# INLINE member #-}

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
notMember :: (Eq k, Hashable k) => k -> MonoidalHashMap k a -> Bool
notMember k = not . M.member k . unpack
{-# INLINE notMember #-}

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> MonoidalHashMap k v -> Maybe v
lookup k = M.lookup k . unpack
{-# INLINE lookup #-}

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or mempty if this map contains no mapping for the key.
lookupM :: (Eq k, Hashable k, Monoid v) => k -> MonoidalHashMap k v -> v
lookupM k = fromMaybe mempty . M.lookup k . unpack
{-# INLINE lookupM #-}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: (Eq k, Hashable k) => k -> MonoidalHashMap k a -> MonoidalHashMap k a
delete k = _Wrapping' MonoidalHashMap %~ M.delete k
{-# INLINE delete #-}

-- | /O(n)/.
-- Return a list of this map's values. The list is produced lazily.
elems :: MonoidalHashMap k a -> [a]
elems = M.elems . unpack
{-# INLINE elems #-}

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
keys :: MonoidalHashMap k a -> [k]
keys = M.keys . unpack
{-# INLINE keys #-}

-- | /O(n*log n)/. Construct a map with the supplied mappings. If the list
-- contains duplicate mappings, values will be replaced.
fromList :: (Eq k, Hashable k) => [(k,a)] -> MonoidalHashMap k a
fromList = pack . M.fromList
{-# INLINE fromList #-}

-- | /O(n*log n)/. Construct a map with the supplied mappings. If the list
-- contains duplicate mappings, values will be merged using the provided combining function.
fromListWith :: (Eq k, Hashable k) => (a -> a -> a) -> [(k,a)] -> MonoidalHashMap k a
fromListWith f = pack . M.fromListWith f
{-# INLINE fromListWith #-}


-- | /O(n*log n)/.  Return a list of this map's elements. The list is produced
-- lazily. The order of its elements is unspecified.
toList :: MonoidalHashMap k a -> [(k,a)]
toList = M.toList . unpack
{-# INLINE toList #-}

-- | /O(log n)/. Insert a value on some key, if it exists replace the value.
insert :: (Hashable k, Eq k)
       => k
       -> a
       -> MonoidalHashMap k a
       -> MonoidalHashMap k a
insert k x = pack
           . M.insert k x
           . unpack

-- | /O(log n)/. Insert a value on some key, if it exists apply the combining function.
insertWith :: (Hashable k, Eq k)
       => (a -> a -> a)
       -> k
       -> a
       -> MonoidalHashMap k a
       -> MonoidalHashMap k a
insertWith f k x = pack
           . M.insertWith f k x
           . unpack

-- | /O(log n)/. Modify a value on some key with a function, if value
-- under key doesn't exist -- use mempty.
modify :: (Monoid a, Hashable k, Eq k)
       => (a -> a)
       -> k -> MonoidalHashMap k a
       -> MonoidalHashMap k a
modify f k = pack
           . M.insertWith (\_ old -> f old) k (f mempty)
           . unpack
{-# INLINE modify #-}

-- | /O(log n)/. Modify a value on some key with a function, providing
-- a default value if that key doesn't exist.
modifyDef :: (Hashable k, Eq k)
          => a -> (a -> a)
          -> k -> MonoidalHashMap k a
          -> MonoidalHashMap k a
modifyDef d f k = pack
                . M.insertWith (\_ old -> f old) k d
                . unpack
{-# INLINE modifyDef #-}

-- | /O(n)/. Map a function to each key of a map, if it will result
-- in duplicated mappings, their values will be merged in unspecified order
mapKeys :: (Hashable k, Eq k, Hashable k', Eq k')
        => (k -> k') -> MonoidalHashMap k a -> MonoidalHashMap k' a
mapKeys f = fromList
          . fmap (\(k, v) -> (f k, v))
          . toList
{-# INLINE mapKeys #-}

-- | /O(n)/ Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: (k -> v -> Bool) -> MonoidalHashMap k v -> MonoidalHashMap k v
filterWithKey pred = pack . M.filterWithKey pred . unpack
{-# INLINE filterWithKey #-}

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> MonoidalHashMap k v1 -> MonoidalHashMap k v2
map f = pack . M.map f . unpack
{-# INLINE map #-}
