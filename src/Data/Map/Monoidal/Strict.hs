{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides a 'Data.Map' variant which uses the value's
-- 'Monoid' instance to accumulate conflicting entries when merging
-- 'Map's.
--
-- While some functions mirroring those of 'Data.Map' are provided
-- here for convenience, more specialized needs will likely want to use
-- either the 'Newtype' or 'Wrapped' instances to manipulate the
-- underlying 'Map'.

module Data.Map.Monoidal.Strict
    ( MonoidalMap(..)
      -- * Often-needed functions
    , singleton
    , size
    , member
    , notMember
    , findWithDefault
    , assocs
    , elems
    , keys
    , (!)
    , (\\)
    , adjust
    , adjustWithKey
    , alter
    , delete
    , deleteAt
    , deleteFindMax
    , deleteFindMin
    , deleteMax
    , deleteMin
    , difference
    , differenceWith
    , differenceWithKey
    , elemAt
    , empty
    , filter
    , filterWithKey
    , findIndex
    , findMax
    , findMin
    , foldMapWithKey
    , foldl
    , foldl'
    , foldlWithKey
    , foldlWithKey'
    , foldr
    , foldr'
    , foldrWithKey
    , foldrWithKey'
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList
    , fromList
    , fromListWith
    , fromListWithKey
    , fromSet
    , insert
    , insertLookupWithKey
    , insertWith
    , insertWithKey
    , intersectionWith
    , intersectionWithKey
    , isProperSubmapOf
    , isProperSubmapOfBy
    , isSubmapOf
    , isSubmapOfBy
    , keysSet
    , lookup
    , lookupGE
    , lookupGT
    , lookupIndex
    , lookupLE
    , lookupLT
    , map
    , mapAccum
    , mapAccumRWithKey
    , mapAccumWithKey
    , mapEither
    , mapEitherWithKey
    , mapKeys
    , mapKeysMonotonic
    , mapKeysWith
    , mapMaybe
    , mapMaybeWithKey
    , mapWithKey
    , maxView
    , maxViewWithKey
    , mergeWithKey
    , minView
    , minViewWithKey
    , null
    , partition
    , partitionWithKey
    , split
    , splitLookup
    , splitRoot
    , toAscList
    , toDescList
    , toList
    , traverseWithKey
    , unionWith
    , unionWithKey
    , unionsWith
    , update
    , updateAt
    , updateLookupWithKey
    , updateMax
    , updateMaxWithKey
    , updateMin
    , updateMinWithKey
    , updateWithKey
    , valid
    -- , showTree
    -- , showTreeWith
    ) where

import Prelude hiding (null, lookup, map, foldl, foldr, filter)

import Data.Coerce (coerce)
import Data.Semigroup
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative (Applicative, pure)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Set (Set)

#if MIN_VERSION_base(4,7,0)
import qualified GHC.Exts  as IsList
#endif

import Control.DeepSeq
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Newtype
import Data.Aeson(FromJSON, ToJSON, FromJSON1, ToJSON1)
#if MIN_VERSION_containers(0,5,9)
import Data.Functor.Classes
#endif

-- | A 'Map' with monoidal accumulation
newtype MonoidalMap k a = MonoidalMap { getMonoidalMap :: M.Map k a }
    deriving (Show, Read, Eq, Ord, NFData,
              Foldable, Traversable,
              FromJSON, ToJSON, FromJSON1, ToJSON1,
              Data, Typeable)

instance Functor (MonoidalMap k) where
  fmap f = coerce ((M.map :: (a -> b) -> M.Map k a -> M.Map k b) f)
  {-# INLINE fmap #-}

#if MIN_VERSION_containers(0,5,9)
deriving instance (Ord k) => Eq1 (MonoidalMap k)
deriving instance (Ord k) => Ord1 (MonoidalMap k)
deriving instance (Show k) => Show1 (MonoidalMap k)
#endif

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
    _Empty = nearly (MonoidalMap M.empty) (M.null . unpack)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalMap k a) where
    type Unwrapped (MonoidalMap k a) = M.Map k a
    _Wrapped' = iso unpack pack
    {-# INLINE _Wrapped' #-}

instance (Ord k, Semigroup a) => Semigroup (MonoidalMap k a) where
    MonoidalMap a <> MonoidalMap b = MonoidalMap $ M.unionWith (<>) a b
    {-# INLINE (<>) #-}

instance (Ord k, Semigroup a) => Monoid (MonoidalMap k a) where
    mempty = MonoidalMap mempty
    {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
    mappend (MonoidalMap a) (MonoidalMap b) = MonoidalMap $ M.unionWith (<>) a b
    {-# INLINE mappend #-}
#endif

instance Newtype (MonoidalMap k a) (M.Map k a) where
    pack = MonoidalMap
    {-# INLINE pack #-}
    unpack (MonoidalMap a) = a
    {-# INLINE unpack #-}

#if MIN_VERSION_base(4,7,0)
instance (Ord k, Semigroup a) => IsList.IsList (MonoidalMap k a) where
    type Item (MonoidalMap k a) = (k, a)
    fromList = MonoidalMap . M.fromListWith (<>)
    {-# INLINE fromList #-}
    toList = M.toList . unpack
    {-# INLINE toList #-}
#endif

-- | /O(1)/. A map with a single element.
singleton :: k -> a -> MonoidalMap k a
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


(!) :: forall k a. Ord k => MonoidalMap k a -> k -> a
(!) = coerce ((M.!) :: M.Map k a -> k -> a)
infixl 9 !

(\\) :: forall k a b. Ord k => MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
(\\) = coerce ((M.\\) :: M.Map k a -> M.Map k b -> M.Map k a)
infixl 9 \\ --

null :: forall k a. MonoidalMap k a -> Bool
null = coerce (M.null :: M.Map k a -> Bool)
{-# INLINE null #-}

lookup :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe a
lookup = coerce (M.lookup :: k -> M.Map k a -> Maybe a)
{-# INLINE lookup #-}

lookupLT :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupLT = coerce (M.lookupLT :: k -> M.Map k a -> Maybe (k,a))
{-# INLINE lookupLT #-}

lookupGT :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupGT = coerce (M.lookupGT :: k -> M.Map k a -> Maybe (k,a))
{-# INLINE lookupGT #-}

lookupLE :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupLE = coerce (M.lookupLE :: k -> M.Map k a -> Maybe (k,a))
{-# INLINE lookupLE #-}

lookupGE :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupGE = coerce (M.lookupGE :: k -> M.Map k a -> Maybe (k,a))
{-# INLINE lookupGE #-}

empty :: forall k a. MonoidalMap k a
empty = coerce (M.empty :: M.Map k a)
{-# INLINE empty #-}

insert :: forall k a. Ord k => k -> a -> MonoidalMap k a -> MonoidalMap k a
insert = coerce (M.insert :: k -> a -> M.Map k a -> M.Map k a)
{-# INLINE insert #-}

insertWith :: forall k a. Ord k => (a -> a -> a) -> k -> a -> MonoidalMap k a -> MonoidalMap k a
insertWith = coerce (M.insertWith :: (a -> a -> a) -> k -> a -> M.Map k a -> M.Map k a)
{-# INLINE insertWith #-}

insertWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> MonoidalMap k a -> MonoidalMap k a
insertWithKey = coerce (M.insertWithKey :: (k -> a -> a -> a) -> k -> a -> M.Map k a -> M.Map k a)
{-# INLINE insertWithKey #-}

insertLookupWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> MonoidalMap k a -> (Maybe a, MonoidalMap k a)
insertLookupWithKey = coerce (M.insertLookupWithKey :: (k -> a -> a -> a) -> k -> a -> M.Map k a -> (Maybe a, M.Map k a))
{-# INLINE insertLookupWithKey #-}

adjust :: forall k a. Ord k => (a -> a) -> k -> MonoidalMap k a -> MonoidalMap k a
adjust = coerce (M.adjust :: (a -> a) -> k -> M.Map k a -> M.Map k a)
{-# INLINE adjust #-}

adjustWithKey :: forall k a. Ord k => (k -> a -> a) -> k -> MonoidalMap k a -> MonoidalMap k a
adjustWithKey = coerce (M.adjustWithKey :: (k -> a -> a) -> k -> M.Map k a -> M.Map k a)
{-# INLINE adjustWithKey #-}

update :: forall k a. Ord k => (a -> Maybe a) -> k -> MonoidalMap k a -> MonoidalMap k a
update = coerce (M.update :: (a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
{-# INLINE update #-}

updateWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> MonoidalMap k a -> MonoidalMap k a
updateWithKey = coerce (M.updateWithKey :: (k -> a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
{-# INLINE updateWithKey #-}

updateLookupWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> MonoidalMap k a -> (Maybe a, MonoidalMap k a)
updateLookupWithKey = coerce (M.updateLookupWithKey :: (k -> a -> Maybe a) -> k -> M.Map k a -> (Maybe a, M.Map k a))
{-# INLINE updateLookupWithKey #-}

alter :: forall k a. Ord k => (Maybe a -> Maybe a) -> k -> MonoidalMap k a -> MonoidalMap k a
alter = coerce (M.alter :: (Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
{-# INLINE alter #-}

unionWith :: forall k a. Ord k => (a -> a -> a) -> MonoidalMap k a -> MonoidalMap k a -> MonoidalMap k a
unionWith = coerce (M.unionWith :: (a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a)
{-# INLINE unionWith #-}

unionWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> MonoidalMap k a -> MonoidalMap k a -> MonoidalMap k a
unionWithKey = coerce (M.unionWithKey :: (k -> a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a)
{-# INLINE unionWithKey #-}

unionsWith :: forall k a. Ord k => (a -> a -> a) -> [MonoidalMap k a] -> MonoidalMap k a
unionsWith = coerce (M.unionsWith :: (a -> a -> a) -> [M.Map k a] -> M.Map k a)
{-# INLINE unionsWith #-}

difference :: forall k a b. Ord k => MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
difference = (\\)
{-# INLINE difference #-}

differenceWith :: forall k a b. Ord k => (a -> b -> Maybe a) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
differenceWith = coerce (M.differenceWith :: (a -> b -> Maybe a) -> M.Map k a -> M.Map k b -> M.Map k a)
{-# INLINE differenceWith #-}

differenceWithKey :: forall k a b. Ord k => (k -> a -> b -> Maybe a) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
differenceWithKey = coerce (M.differenceWithKey :: (k -> a -> b -> Maybe a) -> M.Map k a -> M.Map k b -> M.Map k a)
{-# INLINE differenceWithKey #-}

intersectionWith :: forall k a b c. Ord k => (a -> b -> c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
intersectionWith = coerce (M.intersectionWith :: (a -> b -> c) -> M.Map k a -> M.Map k b -> M.Map k c)
{-# INLINE intersectionWith #-}

intersectionWithKey :: forall k a b c. Ord k => (k -> a -> b -> c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
intersectionWithKey = coerce (M.intersectionWithKey :: (k -> a -> b -> c) -> M.Map k a -> M.Map k b -> M.Map k c)
{-# INLINE intersectionWithKey #-}

mergeWithKey :: forall k a b c. Ord k => (k -> a -> b -> Maybe c) -> (MonoidalMap k a -> MonoidalMap k c) -> (MonoidalMap k b -> MonoidalMap k c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
mergeWithKey = coerce (M.mergeWithKey :: (k -> a -> b -> Maybe c) -> (M.Map k a -> M.Map k c) -> (M.Map k b -> M.Map k c) -> M.Map k a -> M.Map k b -> M.Map k c)
{-# INLINE mergeWithKey #-}

map :: (a -> b) -> MonoidalMap k a -> MonoidalMap k b
map = fmap
{-# INLINE map #-}

mapWithKey :: forall k a  b. (k -> a -> b) -> MonoidalMap k a -> MonoidalMap k b
mapWithKey = coerce (M.mapWithKey :: (k -> a -> b) -> M.Map k a -> M.Map k b)
{-# INLINE mapWithKey #-}

traverseWithKey :: Applicative t => (k -> a -> t b) -> MonoidalMap k a -> t (MonoidalMap k b)
traverseWithKey = itraverse
{-# INLINE traverseWithKey #-}

mapAccum :: forall k a b c. (a -> b -> (a, c)) -> a -> MonoidalMap k b -> (a, MonoidalMap k c)
mapAccum = coerce (M.mapAccum :: (a -> b -> (a, c)) -> a -> M.Map k b -> (a, M.Map k c))
{-# INLINE mapAccum #-}

mapAccumWithKey :: forall k a b c. (a -> k -> b -> (a, c)) -> a -> MonoidalMap k b -> (a, MonoidalMap k c)
mapAccumWithKey = coerce (M.mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> M.Map k b -> (a, M.Map k c))
{-# INLINE mapAccumWithKey #-}

mapAccumRWithKey :: forall k a b c. (a -> k -> b -> (a, c)) -> a -> MonoidalMap k b -> (a, MonoidalMap k c)
mapAccumRWithKey = coerce (M.mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> M.Map k b -> (a, M.Map k c))
{-# INLINE mapAccumRWithKey #-}

mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> MonoidalMap k1 a -> MonoidalMap k2 a
mapKeys = coerce (M.mapKeys :: (k1 -> k2) -> M.Map k1 a -> M.Map k2 a)
{-# INLINE mapKeys #-}

mapKeysWith :: forall k1 k2 a. Ord k2 => (a -> a -> a) -> (k1 -> k2) -> MonoidalMap k1 a -> MonoidalMap k2 a
mapKeysWith = coerce (M.mapKeysWith :: (a -> a -> a) -> (k1 -> k2) -> M.Map k1 a -> M.Map k2 a)
{-# INLINE mapKeysWith #-}

mapKeysMonotonic :: forall k1 k2 a. (k1 -> k2) -> MonoidalMap k1 a -> MonoidalMap k2 a
mapKeysMonotonic = coerce (M.mapKeysMonotonic :: (k1 -> k2) -> M.Map k1 a -> M.Map k2 a)
{-# INLINE mapKeysMonotonic #-}

foldr :: forall k a b. (a -> b -> b) -> b -> MonoidalMap k a -> b
foldr = coerce (M.foldr :: (a -> b -> b) -> b -> M.Map k a -> b)
{-# INLINE foldr #-}

foldl :: forall k a b. (a -> b -> a) -> a -> MonoidalMap k b -> a
foldl = coerce (M.foldl :: (a -> b -> a) -> a -> M.Map k b -> a)
{-# INLINE foldl #-}

foldrWithKey :: forall k a b. (k -> a -> b -> b) -> b -> MonoidalMap k a -> b
foldrWithKey = coerce (M.foldrWithKey :: (k -> a -> b -> b) -> b -> M.Map k a -> b)
{-# INLINE foldrWithKey #-}

foldlWithKey :: forall k a b. (a -> k -> b -> a) -> a -> MonoidalMap k b -> a
foldlWithKey = coerce (M.foldlWithKey :: (a -> k -> b -> a) -> a -> M.Map k b -> a)
{-# INLINE foldlWithKey #-}

foldMapWithKey :: forall k a m. Monoid m => (k -> a -> m) -> MonoidalMap k a -> m
foldMapWithKey = coerce (M.foldMapWithKey :: Monoid m => (k -> a -> m) -> M.Map k a -> m)
{-# INLINE foldMapWithKey #-}

foldr' :: forall k a b. (a -> b -> b) -> b -> MonoidalMap k a -> b
foldr' = coerce (M.foldr' :: (a -> b -> b) -> b -> M.Map k a -> b)
{-# INLINE foldr' #-}

foldl' :: forall k a b. (a -> b -> a) -> a -> MonoidalMap k b -> a
foldl' = coerce (M.foldl' :: (a -> b -> a) -> a -> M.Map k b -> a)
{-# INLINE foldl' #-}

foldrWithKey' :: forall k a b. (k -> a -> b -> b) -> b -> MonoidalMap k a -> b
foldrWithKey' = coerce (M.foldrWithKey' :: (k -> a -> b -> b) -> b -> M.Map k a -> b)
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: forall k a b. (a -> k -> b -> a) -> a -> MonoidalMap k b -> a
foldlWithKey' = coerce (M.foldlWithKey' :: (a -> k -> b -> a) -> a -> M.Map k b -> a)
{-# INLINE foldlWithKey' #-}

keysSet :: forall k a. MonoidalMap k a -> Set k
keysSet = coerce (M.keysSet :: M.Map k a -> Set k)
{-# INLINE keysSet #-}

fromSet :: forall k a. (k -> a) -> Set k -> MonoidalMap k a
fromSet = coerce (M.fromSet :: (k -> a) -> Set k -> M.Map k a)
{-# INLINE fromSet #-}

toList :: forall k a. MonoidalMap k a -> [(k, a)]
toList = coerce (M.toList :: M.Map k a -> [(k, a)])
{-# INLINE toList #-}

fromList :: forall k a. Ord k => [(k, a)] -> MonoidalMap k a
fromList = coerce (M.fromList :: [(k, a)] -> M.Map k a)
{-# INLINE fromList #-}

fromListWith :: forall k a. Ord k => (a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromListWith = coerce (M.fromListWith :: (a -> a -> a) -> [(k, a)] -> M.Map k a)
{-# INLINE fromListWith #-}

fromListWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromListWithKey = coerce (M.fromListWithKey :: (k -> a -> a -> a) -> [(k, a)] -> M.Map k a)
{-# INLINE fromListWithKey #-}

toAscList :: forall k a. MonoidalMap k a -> [(k, a)]
toAscList = coerce (M.toAscList :: M.Map k a -> [(k, a)])
{-# INLINE toAscList #-}

toDescList :: forall k a. MonoidalMap k a -> [(k, a)]
toDescList = coerce (M.toDescList :: M.Map k a -> [(k, a)])
{-# INLINE toDescList #-}

fromAscList :: forall k a. Eq k => [(k, a)] -> MonoidalMap k a
fromAscList = coerce (M.fromAscList :: [(k, a)] -> M.Map k a)
{-# INLINE fromAscList #-}

fromAscListWith :: forall k a. Eq k => (a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromAscListWith = coerce (M.fromAscListWith :: (a -> a -> a) -> [(k, a)] -> M.Map k a)
{-# INLINE fromAscListWith #-}

fromAscListWithKey :: forall k a. Eq k => (k -> a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromAscListWithKey = coerce (M.fromAscListWithKey :: (k -> a -> a -> a) -> [(k, a)] -> M.Map k a)
{-# INLINE fromAscListWithKey #-}

fromDistinctAscList :: forall k a. [(k, a)] -> MonoidalMap k a
fromDistinctAscList = coerce (M.fromDistinctAscList :: [(k, a)] -> M.Map k a)
{-# INLINE fromDistinctAscList #-}

filter :: forall k a. (a -> Bool) -> MonoidalMap k a -> MonoidalMap k a
filter = coerce (M.filter :: (a -> Bool) -> M.Map k a -> M.Map k a)
{-# INLINE filter #-}

filterWithKey :: forall k a. (k -> a -> Bool) -> MonoidalMap k a -> MonoidalMap k a
filterWithKey = coerce (M.filterWithKey :: (k -> a -> Bool) -> M.Map k a -> M.Map k a)
{-# INLINE filterWithKey #-}

partition :: forall k a. (a -> Bool) -> MonoidalMap k a -> (MonoidalMap k a, MonoidalMap k a)
partition = coerce (M.partition :: (a -> Bool) -> M.Map k a -> (M.Map k a, M.Map k a))
{-# INLINE partition #-}

partitionWithKey :: forall k a. (k -> a -> Bool) -> MonoidalMap k a -> (MonoidalMap k a, MonoidalMap k a)
partitionWithKey = coerce (M.partitionWithKey :: (k -> a -> Bool) -> M.Map k a -> (M.Map k a, M.Map k a))
{-# INLINE partitionWithKey #-}

mapMaybe :: forall k a b. (a -> Maybe b) -> MonoidalMap k a -> MonoidalMap k b
mapMaybe = coerce (M.mapMaybe :: (a -> Maybe b) -> M.Map k a -> M.Map k b)
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: forall k a b. (k -> a -> Maybe b) -> MonoidalMap k a -> MonoidalMap k b
mapMaybeWithKey = coerce (M.mapMaybeWithKey :: (k -> a -> Maybe b) -> M.Map k a -> M.Map k b)
{-# INLINE mapMaybeWithKey #-}

mapEither :: forall k a b c. (a -> Either b c) -> MonoidalMap k a -> (MonoidalMap k b, MonoidalMap k c)
mapEither = coerce (M.mapEither :: (a -> Either b c) -> M.Map k a -> (M.Map k b, M.Map k c))
{-# INLINE mapEither #-}

mapEitherWithKey :: forall k a b c. (k -> a -> Either b c) -> MonoidalMap k a -> (MonoidalMap k b, MonoidalMap k c)
mapEitherWithKey = coerce (M.mapEitherWithKey :: (k -> a -> Either b c) -> M.Map k a -> (M.Map k b, M.Map k c))
{-# INLINE mapEitherWithKey #-}

split :: forall k a. Ord k => k -> MonoidalMap k a -> (MonoidalMap k a, MonoidalMap k a)
split = coerce (M.split :: k -> M.Map k a -> (M.Map k a, M.Map k a))
{-# INLINE split #-}

splitLookup :: forall k a. Ord k => k -> MonoidalMap k a -> (MonoidalMap k a, Maybe a, MonoidalMap k a)
splitLookup = coerce (M.splitLookup :: k -> M.Map k a -> (M.Map k a, Maybe a, M.Map k a))
{-# INLINE splitLookup #-}

splitRoot :: forall k a. MonoidalMap k a -> [MonoidalMap k a]
splitRoot = coerce (M.splitRoot :: M.Map k a -> [M.Map k a])
{-# INLINE splitRoot #-}

isSubmapOf :: forall k a. (Ord k, Eq a) => MonoidalMap k a -> MonoidalMap k a -> Bool
isSubmapOf = coerce (M.isSubmapOf :: M.Map k a -> M.Map k a -> Bool)
{-# INLINE isSubmapOf #-}

isSubmapOfBy :: forall k a b. Ord k => (a -> b -> Bool) -> MonoidalMap k a -> MonoidalMap k b -> Bool
isSubmapOfBy = coerce (M.isSubmapOfBy :: (a -> b -> Bool) -> M.Map k a -> M.Map k b -> Bool)
{-# INLINE isSubmapOfBy #-}

isProperSubmapOf :: forall k a. (Ord k, Eq a) => MonoidalMap k a -> MonoidalMap k a -> Bool
isProperSubmapOf = coerce (M.isProperSubmapOf :: M.Map k a -> M.Map k a -> Bool)
{-# INLINE isProperSubmapOf #-}

isProperSubmapOfBy :: forall k a b. Ord k => (a -> b -> Bool) -> MonoidalMap k a -> MonoidalMap k b -> Bool
isProperSubmapOfBy = coerce (M.isProperSubmapOfBy :: (a -> b -> Bool) -> M.Map k a -> M.Map k b -> Bool)
{-# INLINE isProperSubmapOfBy #-}

lookupIndex :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe Int
lookupIndex = coerce (M.lookupIndex :: k -> M.Map k a -> Maybe Int)
{-# INLINE lookupIndex #-}

findIndex :: forall k a. Ord k => k -> MonoidalMap k a -> Int
findIndex = coerce (M.findIndex :: k -> M.Map k a -> Int)
{-# INLINE findIndex #-}

elemAt :: forall k a. Int -> MonoidalMap k a -> (k, a)
elemAt = coerce (M.elemAt :: Int -> M.Map k a -> (k, a))
{-# INLINE elemAt #-}

updateAt :: forall k a. (k -> a -> Maybe a) -> Int -> MonoidalMap k a -> MonoidalMap k a
updateAt = coerce (M.updateAt :: (k -> a -> Maybe a) -> Int -> M.Map k a -> M.Map k a)
{-# INLINE updateAt #-}

deleteAt :: forall k a. Int -> MonoidalMap k a -> MonoidalMap k a
deleteAt = coerce (M.deleteAt :: Int -> M.Map k a -> M.Map k a)
{-# INLINE deleteAt #-}

findMin :: forall k a. MonoidalMap k a -> (k, a)
findMin = coerce (M.findMin :: M.Map k a -> (k, a))
{-# INLINE findMin #-}

findMax :: forall k a. MonoidalMap k a -> (k, a)
findMax = coerce (M.findMax :: M.Map k a -> (k, a))
{-# INLINE findMax #-}

deleteMin :: forall k a. MonoidalMap k a -> MonoidalMap k a
deleteMin = coerce (M.deleteMin :: M.Map k a -> M.Map k a)
{-# INLINE deleteMin #-}

deleteMax :: forall k a. MonoidalMap k a -> MonoidalMap k a
deleteMax = coerce (M.deleteMax :: M.Map k a -> M.Map k a)
{-# INLINE deleteMax #-}

deleteFindMin :: forall k a. MonoidalMap k a -> ((k, a), MonoidalMap k a)
deleteFindMin = coerce (M.deleteFindMin :: M.Map k a -> ((k, a), M.Map k a))
{-# INLINE deleteFindMin #-}

deleteFindMax :: forall k a. MonoidalMap k a -> ((k, a), MonoidalMap k a)
deleteFindMax = coerce (M.deleteFindMax :: M.Map k a -> ((k, a), M.Map k a))
{-# INLINE deleteFindMax #-}

updateMin :: forall k a. (a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMin = coerce (M.updateMin :: (a -> Maybe a) -> M.Map k a -> M.Map k a)
{-# INLINE updateMin #-}

updateMax :: forall k a. (a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMax = coerce (M.updateMax :: (a -> Maybe a) -> M.Map k a -> M.Map k a)
{-# INLINE updateMax #-}

updateMinWithKey :: forall k a. (k -> a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMinWithKey = coerce (M.updateMinWithKey :: (k -> a -> Maybe a) -> M.Map k a -> M.Map k a)
{-# INLINE updateMinWithKey #-}

updateMaxWithKey :: forall k a. (k -> a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMaxWithKey = coerce (M.updateMaxWithKey :: (k -> a -> Maybe a) -> M.Map k a -> M.Map k a)
{-# INLINE updateMaxWithKey #-}

minView :: forall k a. MonoidalMap k a -> Maybe (a, MonoidalMap k a)
minView = coerce (M.minView :: M.Map k a -> Maybe (a, M.Map k a))
{-# INLINE minView #-}

maxView :: forall k a. MonoidalMap k a -> Maybe (a, MonoidalMap k a)
maxView = coerce (M.maxView :: M.Map k a -> Maybe (a, M.Map k a))
{-# INLINE maxView #-}

minViewWithKey :: forall k a. MonoidalMap k a -> Maybe ((k, a), MonoidalMap k a)
minViewWithKey = coerce (M.minViewWithKey :: M.Map k a -> Maybe ((k, a), M.Map k a))
{-# INLINE minViewWithKey #-}

maxViewWithKey :: forall k a. MonoidalMap k a -> Maybe ((k, a), MonoidalMap k a)
maxViewWithKey = coerce (M.maxViewWithKey :: M.Map k a -> Maybe ((k, a), M.Map k a))
{-# INLINE maxViewWithKey #-}

-- showTree :: forall k a. (Show k, Show a) => MonoidalMap k a -> String
-- showTree = coerce (M.showTree :: (Show k, Show a) => M.Map k a -> String)
-- {-# INLINE showTree #-}

-- showTreeWith :: forall k a. (k -> a -> String) -> Bool -> Bool -> MonoidalMap k a -> String
-- showTreeWith = coerce (M.showTreeWith :: (k -> a -> String) -> Bool -> Bool -> M.Map k a -> String)
-- {-# INLINE showTreeWith #-}

valid :: forall k a. Ord k => MonoidalMap k a -> Bool
valid = coerce (M.valid :: Ord k => M.Map k a -> Bool)
{-# INLINE valid #-}


