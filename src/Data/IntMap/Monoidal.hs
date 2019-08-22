{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides a 'Data.IntMap' variant which uses the value's
-- 'Monoid' instance to accumulate conflicting entries when merging
-- 'Map's.
--
-- While some functions mirroring those of 'Data.IntMap' are provided
-- here for convenience, more specialized needs will likely want to use
-- either the 'Newtype' or 'Wrapped' instances to manipulate the
-- underlying 'Map'.

module Data.IntMap.Monoidal
    ( MonoidalIntMap(..)
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
--    , deleteAt
    , deleteFindMax
    , deleteFindMin
    , deleteMax
    , deleteMin
    , difference
    , differenceWith
    , differenceWithKey
--    , elemAt
    , empty
    , filter
    , filterWithKey
--    , findIndex
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
    , fromDistinctList
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
--    , lookupIndex
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
--    , updateAt
    , updateLookupWithKey
    , updateMax
    , updateMaxWithKey
    , updateMin
    , updateMinWithKey
    , updateWithKey
--    , valid
    -- , showTree
    -- , showTreeWith
    ) where

import Prelude hiding (null, lookup, map, foldl, foldr, filter)

import Data.Coerce (coerce)
import Data.IntSet (IntSet)
import Data.Semigroup
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative (Applicative, pure)
import Data.Data (Data)
import Data.Typeable (Typeable)

#if MIN_VERSION_base(4,7,0)
import qualified GHC.Exts as IsList
#endif

import Control.DeepSeq
import qualified Data.IntMap as M
import Control.Lens
import Control.Newtype
import Data.Aeson(FromJSON, ToJSON, FromJSON1, ToJSON1)
#if MIN_VERSION_containers(0,5,9)
import Data.Functor.Classes
#endif
import Data.Align

-- | An 'IntMap' with monoidal accumulation
newtype MonoidalIntMap a = MonoidalIntMap { getMonoidalIntMap :: M.IntMap a }
    deriving (Show, Read, Functor, Eq, Ord, NFData,
              Foldable, Traversable,
              FromJSON, ToJSON, FromJSON1, ToJSON1,
              Data, Typeable, Align
#if MIN_VERSION_these(0,8,0)
             , Semialign
#endif
             )

#if MIN_VERSION_containers(0,5,9)
deriving instance Eq1 MonoidalIntMap
deriving instance Ord1 MonoidalIntMap
deriving instance Show1 MonoidalIntMap
#endif

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
    _Empty = nearly (MonoidalIntMap M.empty) (M.null . unpack)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalIntMap a) where
    type Unwrapped (MonoidalIntMap a) = M.IntMap a
    _Wrapped' = iso unpack pack
    {-# INLINE _Wrapped' #-}

instance Semigroup a => Semigroup (MonoidalIntMap a) where
    MonoidalIntMap a <> MonoidalIntMap b = MonoidalIntMap $ M.unionWith (<>) a b
    {-# INLINE (<>) #-}

instance Semigroup a => Monoid (MonoidalIntMap a) where
    mempty = MonoidalIntMap mempty
    {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
    mappend (MonoidalIntMap a) (MonoidalIntMap b) = MonoidalIntMap $ M.unionWith (<>) a b
    {-# INLINE mappend #-}
#endif

instance Newtype (MonoidalIntMap a) (M.IntMap a) where
    pack = MonoidalIntMap
    {-# INLINE pack #-}
    unpack (MonoidalIntMap a) = a
    {-# INLINE unpack #-}

#if MIN_VERSION_base(4,7,0)
instance Semigroup a => IsList.IsList (MonoidalIntMap a) where
    type Item (MonoidalIntMap a) = (Int, a)
    fromList = MonoidalIntMap . M.fromListWith (<>)
    {-# INLINE fromList #-}
    toList = M.toList . unpack
    {-# INLINE toList #-}
#endif

-- | /O(1)/. A map with a single element.
singleton :: Int -> a -> MonoidalIntMap a
singleton k a = MonoidalIntMap $ M.singleton k a
{-# INLINE singleton #-}

-- | /O(1)/. The number of elements in the map.
size :: MonoidalIntMap a -> Int
size = M.size . unpack
{-# INLINE size #-}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: Int -> MonoidalIntMap a -> Bool
member k = M.member k . unpack
{-# INLINE member #-}

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
notMember :: Int -> MonoidalIntMap a -> Bool
notMember k = not . M.member k . unpack
{-# INLINE notMember #-}

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
findWithDefault :: forall a. a -> Int -> MonoidalIntMap a -> a
findWithDefault def k = M.findWithDefault def k . unpack
{-# INLINE findWithDefault #-}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: Int -> MonoidalIntMap a -> MonoidalIntMap a
delete k = _Wrapping' MonoidalIntMap %~ M.delete k
{-# INLINE delete #-}

-- | /O(n)/. Return all elements of the map and their keys
assocs :: MonoidalIntMap a -> [(Int,a)]
assocs = M.assocs . unpack
{-# INLINE assocs #-}

-- | /O(n)/. Return all elements of the map in the ascending order of their
-- keys. Subject to list fusion.
elems :: MonoidalIntMap a -> [a]
elems = M.elems . unpack
{-# INLINE elems #-}

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
keys :: MonoidalIntMap a -> [Int]
keys = M.keys . unpack
{-# INLINE keys #-}

(!) :: forall a. MonoidalIntMap a -> Int -> a
(!) = coerce ((M.!) :: M.IntMap a -> Int -> a)
infixl 9 !

(\\) :: forall a b. MonoidalIntMap a -> MonoidalIntMap b -> MonoidalIntMap a
(\\) = coerce ((M.\\) :: M.IntMap a -> M.IntMap b -> M.IntMap a)
infixl 9 \\ --

null :: forall a. MonoidalIntMap a -> Bool
null = coerce (M.null :: M.IntMap a -> Bool)
{-# INLINE null #-}

lookup :: forall a. Int -> MonoidalIntMap a -> Maybe a
lookup = coerce (M.lookup :: Int -> M.IntMap a -> Maybe a)
{-# INLINE lookup #-}

lookupLT :: forall a. Int -> MonoidalIntMap a -> Maybe (Int, a)
lookupLT = coerce (M.lookupLT :: Int -> M.IntMap a -> Maybe (Int,a))
{-# INLINE lookupLT #-}

lookupGT :: forall a. Int -> MonoidalIntMap a -> Maybe (Int, a)
lookupGT = coerce (M.lookupGT :: Int -> M.IntMap a -> Maybe (Int,a))
{-# INLINE lookupGT #-}

lookupLE :: forall a. Int -> MonoidalIntMap a -> Maybe (Int, a)
lookupLE = coerce (M.lookupLE :: Int -> M.IntMap a -> Maybe (Int,a))
{-# INLINE lookupLE #-}

lookupGE :: forall a. Int -> MonoidalIntMap a -> Maybe (Int, a)
lookupGE = coerce (M.lookupGE :: Int -> M.IntMap a -> Maybe (Int,a))
{-# INLINE lookupGE #-}

empty :: forall a. MonoidalIntMap a
empty = coerce (M.empty :: M.IntMap a)
{-# INLINE empty #-}

insert :: forall a. Int -> a -> MonoidalIntMap a -> MonoidalIntMap a
insert = coerce (M.insert :: Int -> a -> M.IntMap a -> M.IntMap a)
{-# INLINE insert #-}

insertWith :: forall a. (a -> a -> a) -> Int -> a -> MonoidalIntMap a -> MonoidalIntMap a
insertWith = coerce (M.insertWith :: (a -> a -> a) -> Int -> a -> M.IntMap a -> M.IntMap a)
{-# INLINE insertWith #-}

insertWithKey :: forall a. (Int -> a -> a -> a) -> Int -> a -> MonoidalIntMap a -> MonoidalIntMap a
insertWithKey = coerce (M.insertWithKey :: (Int -> a -> a -> a) -> Int -> a -> M.IntMap a -> M.IntMap a)
{-# INLINE insertWithKey #-}

insertLookupWithKey :: forall a. (Int -> a -> a -> a) -> Int -> a -> MonoidalIntMap a -> (Maybe a, MonoidalIntMap a)
insertLookupWithKey = coerce (M.insertLookupWithKey :: (Int -> a -> a -> a) -> Int -> a -> M.IntMap a -> (Maybe a, M.IntMap a))
{-# INLINE insertLookupWithKey #-}

adjust :: forall a. (a -> a) -> Int -> MonoidalIntMap a -> MonoidalIntMap a
adjust = coerce (M.adjust :: (a -> a) -> Int -> M.IntMap a -> M.IntMap a)
{-# INLINE adjust #-}

adjustWithKey :: forall a. (Int -> a -> a) -> Int -> MonoidalIntMap a -> MonoidalIntMap a
adjustWithKey = coerce (M.adjustWithKey :: (Int -> a -> a) -> Int -> M.IntMap a -> M.IntMap a)
{-# INLINE adjustWithKey #-}

update :: forall a. (a -> Maybe a) -> Int -> MonoidalIntMap a -> MonoidalIntMap a
update = coerce (M.update :: (a -> Maybe a) -> Int -> M.IntMap a -> M.IntMap a)
{-# INLINE update #-}

updateWithKey :: forall a. (Int -> a -> Maybe a) -> Int -> MonoidalIntMap a -> MonoidalIntMap a
updateWithKey = coerce (M.updateWithKey :: (Int -> a -> Maybe a) -> Int -> M.IntMap a -> M.IntMap a)
{-# INLINE updateWithKey #-}

updateLookupWithKey :: forall a. (Int -> a -> Maybe a) -> Int -> MonoidalIntMap a -> (Maybe a, MonoidalIntMap a)
updateLookupWithKey = coerce (M.updateLookupWithKey :: (Int -> a -> Maybe a) -> Int -> M.IntMap a -> (Maybe a, M.IntMap a))
{-# INLINE updateLookupWithKey #-}

alter :: forall a. (Maybe a -> Maybe a) -> Int -> MonoidalIntMap a -> MonoidalIntMap a
alter = coerce (M.alter :: (Maybe a -> Maybe a) -> Int -> M.IntMap a -> M.IntMap a)
{-# INLINE alter #-}

unionWith :: forall a. (a -> a -> a) -> MonoidalIntMap a -> MonoidalIntMap a -> MonoidalIntMap a
unionWith = coerce (M.unionWith :: (a -> a -> a) -> M.IntMap a -> M.IntMap a -> M.IntMap a)
{-# INLINE unionWith #-}

unionWithKey :: forall a. (Int -> a -> a -> a) -> MonoidalIntMap a -> MonoidalIntMap a -> MonoidalIntMap a
unionWithKey = coerce (M.unionWithKey :: (Int -> a -> a -> a) -> M.IntMap a -> M.IntMap a -> M.IntMap a)
{-# INLINE unionWithKey #-}

unionsWith :: forall a. (a -> a -> a) -> [MonoidalIntMap a] -> MonoidalIntMap a
unionsWith = coerce (M.unionsWith :: (a -> a -> a) -> [M.IntMap a] -> M.IntMap a)
{-# INLINE unionsWith #-}

difference :: forall a b. MonoidalIntMap a -> MonoidalIntMap b -> MonoidalIntMap a
difference = (\\)
{-# INLINE difference #-}

differenceWith :: forall a b. (a -> b -> Maybe a) -> MonoidalIntMap a -> MonoidalIntMap b -> MonoidalIntMap a
differenceWith = coerce (M.differenceWith :: (a -> b -> Maybe a) -> M.IntMap a -> M.IntMap b -> M.IntMap a)
{-# INLINE differenceWith #-}

differenceWithKey :: forall a b. (Int -> a -> b -> Maybe a) -> MonoidalIntMap a -> MonoidalIntMap b -> MonoidalIntMap a
differenceWithKey = coerce (M.differenceWithKey :: (Int -> a -> b -> Maybe a) -> M.IntMap a -> M.IntMap b -> M.IntMap a)
{-# INLINE differenceWithKey #-}

intersectionWith :: forall a b c. (a -> b -> c) -> MonoidalIntMap a -> MonoidalIntMap b -> MonoidalIntMap c
intersectionWith = coerce (M.intersectionWith :: (a -> b -> c) -> M.IntMap a -> M.IntMap b -> M.IntMap c)
{-# INLINE intersectionWith #-}

intersectionWithKey :: forall a b c. (Int -> a -> b -> c) -> MonoidalIntMap a -> MonoidalIntMap b -> MonoidalIntMap c
intersectionWithKey = coerce (M.intersectionWithKey :: (Int -> a -> b -> c) -> M.IntMap a -> M.IntMap b -> M.IntMap c)
{-# INLINE intersectionWithKey #-}

mergeWithKey :: forall a b c. (Int -> a -> b -> Maybe c) -> (MonoidalIntMap a -> MonoidalIntMap c) -> (MonoidalIntMap b -> MonoidalIntMap c) -> MonoidalIntMap a -> MonoidalIntMap b -> MonoidalIntMap c
mergeWithKey = coerce (M.mergeWithKey :: (Int -> a -> b -> Maybe c) -> (M.IntMap a -> M.IntMap c) -> (M.IntMap b -> M.IntMap c) -> M.IntMap a -> M.IntMap b -> M.IntMap c)
{-# INLINE mergeWithKey #-}

map :: (a -> b) -> MonoidalIntMap a -> MonoidalIntMap b
map = fmap
{-# INLINE map #-}

mapWithKey :: forall a  b. (Int -> a -> b) -> MonoidalIntMap a -> MonoidalIntMap b
mapWithKey = coerce (M.mapWithKey :: (Int -> a -> b) -> M.IntMap a -> M.IntMap b)
{-# INLINE mapWithKey #-}

traverseWithKey :: Applicative t => (Int -> a -> t b) -> MonoidalIntMap a -> t (MonoidalIntMap b)
traverseWithKey = itraverse
{-# INLINE traverseWithKey #-}

mapAccum :: forall a b c. (a -> b -> (a, c)) -> a -> MonoidalIntMap b -> (a, MonoidalIntMap c)
mapAccum = coerce (M.mapAccum :: (a -> b -> (a, c)) -> a -> M.IntMap b -> (a, M.IntMap c))
{-# INLINE mapAccum #-}

mapAccumWithKey :: forall a b c. (a -> Int -> b -> (a, c)) -> a -> MonoidalIntMap b -> (a, MonoidalIntMap c)
mapAccumWithKey = coerce (M.mapAccumWithKey :: (a -> Int -> b -> (a, c)) -> a -> M.IntMap b -> (a, M.IntMap c))
{-# INLINE mapAccumWithKey #-}

mapAccumRWithKey :: forall a b c. (a -> Int -> b -> (a, c)) -> a -> MonoidalIntMap b -> (a, MonoidalIntMap c)
mapAccumRWithKey = coerce (M.mapAccumRWithKey :: (a -> Int -> b -> (a, c)) -> a -> M.IntMap b -> (a, M.IntMap c))
{-# INLINE mapAccumRWithKey #-}

mapKeys :: forall a. (Int -> Int) -> MonoidalIntMap a -> MonoidalIntMap a
mapKeys = coerce (M.mapKeys :: (Int -> Int) -> M.IntMap a -> M.IntMap a)
{-# INLINE mapKeys #-}

mapKeysWith :: forall a. (a -> a -> a) -> (Int -> Int) -> MonoidalIntMap a -> MonoidalIntMap a
mapKeysWith = coerce (M.mapKeysWith :: (a -> a -> a) -> (Int -> Int) -> M.IntMap a -> M.IntMap a)
{-# INLINE mapKeysWith #-}

mapKeysMonotonic :: forall a. (Int -> Int) -> MonoidalIntMap a -> MonoidalIntMap a
mapKeysMonotonic = coerce (M.mapKeysMonotonic :: (Int -> Int) -> M.IntMap a -> M.IntMap a)
{-# INLINE mapKeysMonotonic #-}

foldr :: forall a b. (a -> b -> b) -> b -> MonoidalIntMap a -> b
foldr = coerce (M.foldr :: (a -> b -> b) -> b -> M.IntMap a -> b)
{-# INLINE foldr #-}

foldl :: forall a b. (a -> b -> a) -> a -> MonoidalIntMap b -> a
foldl = coerce (M.foldl :: (a -> b -> a) -> a -> M.IntMap b -> a)
{-# INLINE foldl #-}

foldrWithKey :: forall a b. (Int -> a -> b -> b) -> b -> MonoidalIntMap a -> b
foldrWithKey = coerce (M.foldrWithKey :: (Int -> a -> b -> b) -> b -> M.IntMap a -> b)
{-# INLINE foldrWithKey #-}

foldlWithKey :: forall a b. (a -> Int -> b -> a) -> a -> MonoidalIntMap b -> a
foldlWithKey = coerce (M.foldlWithKey :: (a -> Int -> b -> a) -> a -> M.IntMap b -> a)
{-# INLINE foldlWithKey #-}

foldMapWithKey :: forall a m. Monoid m => (Int -> a -> m) -> MonoidalIntMap a -> m
foldMapWithKey = coerce (M.foldMapWithKey :: Monoid m => (Int -> a -> m) -> M.IntMap a -> m)
{-# INLINE foldMapWithKey #-}

foldr' :: forall a b. (a -> b -> b) -> b -> MonoidalIntMap a -> b
foldr' = coerce (M.foldr' :: (a -> b -> b) -> b -> M.IntMap a -> b)
{-# INLINE foldr' #-}

foldl' :: forall a b. (a -> b -> a) -> a -> MonoidalIntMap b -> a
foldl' = coerce (M.foldl' :: (a -> b -> a) -> a -> M.IntMap b -> a)
{-# INLINE foldl' #-}

foldrWithKey' :: forall a b. (Int -> a -> b -> b) -> b -> MonoidalIntMap a -> b
foldrWithKey' = coerce (M.foldrWithKey' :: (Int -> a -> b -> b) -> b -> M.IntMap a -> b)
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: forall a b. (a -> Int -> b -> a) -> a -> MonoidalIntMap b -> a
foldlWithKey' = coerce (M.foldlWithKey' :: (a -> Int -> b -> a) -> a -> M.IntMap b -> a)
{-# INLINE foldlWithKey' #-}

keysSet :: forall a. MonoidalIntMap a -> IntSet
keysSet = coerce (M.keysSet :: M.IntMap a -> IntSet)
{-# INLINE keysSet #-}

fromSet :: forall a. (Int -> a) -> IntSet -> MonoidalIntMap a
fromSet = coerce (M.fromSet :: (Int -> a) -> IntSet -> M.IntMap a)
{-# INLINE fromSet #-}

toList :: forall a. MonoidalIntMap a -> [(Int, a)]
toList = coerce (M.toList :: M.IntMap a -> [(Int, a)])
{-# INLINE toList #-}

fromList :: forall a. [(Int, a)] -> MonoidalIntMap a
fromList = coerce (M.fromList :: [(Int, a)] -> M.IntMap a)
{-# INLINE fromList #-}

fromListWith :: forall a. (a -> a -> a) -> [(Int, a)] -> MonoidalIntMap a
fromListWith = coerce (M.fromListWith :: (a -> a -> a) -> [(Int, a)] -> M.IntMap a)
{-# INLINE fromListWith #-}

fromListWithKey :: forall a. (Int -> a -> a -> a) -> [(Int, a)] -> MonoidalIntMap a
fromListWithKey = coerce (M.fromListWithKey :: (Int -> a -> a -> a) -> [(Int, a)] -> M.IntMap a)
{-# INLINE fromListWithKey #-}

toAscList :: forall a. MonoidalIntMap a -> [(Int, a)]
toAscList = coerce (M.toAscList :: M.IntMap a -> [(Int, a)])
{-# INLINE toAscList #-}

toDescList :: forall a. MonoidalIntMap a -> [(Int, a)]
toDescList = coerce (M.toDescList :: M.IntMap a -> [(Int, a)])
{-# INLINE toDescList #-}

fromAscList :: forall a. [(Int, a)] -> MonoidalIntMap a
fromAscList = coerce (M.fromAscList :: [(Int, a)] -> M.IntMap a)
{-# INLINE fromAscList #-}

fromAscListWith :: forall a. (a -> a -> a) -> [(Int, a)] -> MonoidalIntMap a
fromAscListWith = coerce (M.fromAscListWith :: (a -> a -> a) -> [(Int, a)] -> M.IntMap a)
{-# INLINE fromAscListWith #-}

fromAscListWithKey :: forall a. (Int -> a -> a -> a) -> [(Int, a)] -> MonoidalIntMap a
fromAscListWithKey = coerce (M.fromAscListWithKey :: (Int -> a -> a -> a) -> [(Int, a)] -> M.IntMap a)
{-# INLINE fromAscListWithKey #-}

fromDistinctAscList :: forall a. [(Int, a)] -> MonoidalIntMap a
fromDistinctAscList = coerce (M.fromDistinctAscList :: [(Int, a)] -> M.IntMap a)
{-# INLINE fromDistinctAscList #-}

fromDistinctList :: forall a. [(Int, a)] -> MonoidalIntMap a
fromDistinctList = coerce (M.fromList :: [(Int, a)] -> M.IntMap a)
{-# INLINE fromDistinctList #-}

filter :: forall a. (a -> Bool) -> MonoidalIntMap a -> MonoidalIntMap a
filter = coerce (M.filter :: (a -> Bool) -> M.IntMap a -> M.IntMap a)
{-# INLINE filter #-}

filterWithKey :: forall a. (Int -> a -> Bool) -> MonoidalIntMap a -> MonoidalIntMap a
filterWithKey = coerce (M.filterWithKey :: (Int -> a -> Bool) -> M.IntMap a -> M.IntMap a)
{-# INLINE filterWithKey #-}

partition :: forall a. (a -> Bool) -> MonoidalIntMap a -> (MonoidalIntMap a, MonoidalIntMap a)
partition = coerce (M.partition :: (a -> Bool) -> M.IntMap a -> (M.IntMap a, M.IntMap a))
{-# INLINE partition #-}

partitionWithKey :: forall a. (Int -> a -> Bool) -> MonoidalIntMap a -> (MonoidalIntMap a, MonoidalIntMap a)
partitionWithKey = coerce (M.partitionWithKey :: (Int -> a -> Bool) -> M.IntMap a -> (M.IntMap a, M.IntMap a))
{-# INLINE partitionWithKey #-}

mapMaybe :: forall a b. (a -> Maybe b) -> MonoidalIntMap a -> MonoidalIntMap b
mapMaybe = coerce (M.mapMaybe :: (a -> Maybe b) -> M.IntMap a -> M.IntMap b)
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: forall a b. (Int -> a -> Maybe b) -> MonoidalIntMap a -> MonoidalIntMap b
mapMaybeWithKey = coerce (M.mapMaybeWithKey :: (Int -> a -> Maybe b) -> M.IntMap a -> M.IntMap b)
{-# INLINE mapMaybeWithKey #-}

mapEither :: forall a b c. (a -> Either b c) -> MonoidalIntMap a -> (MonoidalIntMap b, MonoidalIntMap c)
mapEither = coerce (M.mapEither :: (a -> Either b c) -> M.IntMap a -> (M.IntMap b, M.IntMap c))
{-# INLINE mapEither #-}

mapEitherWithKey :: forall a b c. (Int -> a -> Either b c) -> MonoidalIntMap a -> (MonoidalIntMap b, MonoidalIntMap c)
mapEitherWithKey = coerce (M.mapEitherWithKey :: (Int -> a -> Either b c) -> M.IntMap a -> (M.IntMap b, M.IntMap c))
{-# INLINE mapEitherWithKey #-}

split :: forall a. Int -> MonoidalIntMap a -> (MonoidalIntMap a, MonoidalIntMap a)
split = coerce (M.split :: Int -> M.IntMap a -> (M.IntMap a, M.IntMap a))
{-# INLINE split #-}

splitLookup :: forall a. Int -> MonoidalIntMap a -> (MonoidalIntMap a, Maybe a, MonoidalIntMap a)
splitLookup = coerce (M.splitLookup :: Int -> M.IntMap a -> (M.IntMap a, Maybe a, M.IntMap a))
{-# INLINE splitLookup #-}

splitRoot :: forall a. MonoidalIntMap a -> [MonoidalIntMap a]
splitRoot = coerce (M.splitRoot :: M.IntMap a -> [M.IntMap a])
{-# INLINE splitRoot #-}

isSubmapOf :: forall a. Eq a => MonoidalIntMap a -> MonoidalIntMap a -> Bool
isSubmapOf = coerce (M.isSubmapOf :: M.IntMap a -> M.IntMap a -> Bool)
{-# INLINE isSubmapOf #-}

isSubmapOfBy :: forall a b. (a -> b -> Bool) -> MonoidalIntMap a -> MonoidalIntMap b -> Bool
isSubmapOfBy = coerce (M.isSubmapOfBy :: (a -> b -> Bool) -> M.IntMap a -> M.IntMap b -> Bool)
{-# INLINE isSubmapOfBy #-}

isProperSubmapOf :: forall a. Eq a => MonoidalIntMap a -> MonoidalIntMap a -> Bool
isProperSubmapOf = coerce (M.isProperSubmapOf :: M.IntMap a -> M.IntMap a -> Bool)
{-# INLINE isProperSubmapOf #-}

isProperSubmapOfBy :: forall a b. (a -> b -> Bool) -> MonoidalIntMap a -> MonoidalIntMap b -> Bool
isProperSubmapOfBy = coerce (M.isProperSubmapOfBy :: (a -> b -> Bool) -> M.IntMap a -> M.IntMap b -> Bool)
{-# INLINE isProperSubmapOfBy #-}

findMin :: forall a. MonoidalIntMap a -> (Int, a)
findMin = coerce (M.findMin :: M.IntMap a -> (Int, a))
{-# INLINE findMin #-}

findMax :: forall a. MonoidalIntMap a -> (Int, a)
findMax = coerce (M.findMax :: M.IntMap a -> (Int, a))
{-# INLINE findMax #-}

deleteMin :: forall a. MonoidalIntMap a -> MonoidalIntMap a
deleteMin = coerce (M.deleteMin :: M.IntMap a -> M.IntMap a)
{-# INLINE deleteMin #-}

deleteMax :: forall a. MonoidalIntMap a -> MonoidalIntMap a
deleteMax = coerce (M.deleteMax :: M.IntMap a -> M.IntMap a)
{-# INLINE deleteMax #-}

deleteFindMin :: forall a. MonoidalIntMap a -> ((Int, a), MonoidalIntMap a)
deleteFindMin = coerce (M.deleteFindMin :: M.IntMap a -> ((Int, a), M.IntMap a))
{-# INLINE deleteFindMin #-}

deleteFindMax :: forall a. MonoidalIntMap a -> ((Int, a), MonoidalIntMap a)
deleteFindMax = coerce (M.deleteFindMax :: M.IntMap a -> ((Int, a), M.IntMap a))
{-# INLINE deleteFindMax #-}

updateMin :: forall a. (a -> Maybe a) -> MonoidalIntMap a -> MonoidalIntMap a
updateMin = coerce (M.updateMin :: (a -> Maybe a) -> M.IntMap a -> M.IntMap a)
{-# INLINE updateMin #-}

updateMax :: forall a. (a -> Maybe a) -> MonoidalIntMap a -> MonoidalIntMap a
updateMax = coerce (M.updateMax :: (a -> Maybe a) -> M.IntMap a -> M.IntMap a)
{-# INLINE updateMax #-}

updateMinWithKey :: forall a. (Int -> a -> Maybe a) -> MonoidalIntMap a -> MonoidalIntMap a
updateMinWithKey = coerce (M.updateMinWithKey :: (Int -> a -> Maybe a) -> M.IntMap a -> M.IntMap a)
{-# INLINE updateMinWithKey #-}

updateMaxWithKey :: forall a. (Int -> a -> Maybe a) -> MonoidalIntMap a -> MonoidalIntMap a
updateMaxWithKey = coerce (M.updateMaxWithKey :: (Int -> a -> Maybe a) -> M.IntMap a -> M.IntMap a)
{-# INLINE updateMaxWithKey #-}

minView :: forall a. MonoidalIntMap a -> Maybe (a, MonoidalIntMap a)
minView = coerce (M.minView :: M.IntMap a -> Maybe (a, M.IntMap a))
{-# INLINE minView #-}

maxView :: forall a. MonoidalIntMap a -> Maybe (a, MonoidalIntMap a)
maxView = coerce (M.maxView :: M.IntMap a -> Maybe (a, M.IntMap a))
{-# INLINE maxView #-}

minViewWithKey :: forall a. MonoidalIntMap a -> Maybe ((Int, a), MonoidalIntMap a)
minViewWithKey = coerce (M.minViewWithKey :: M.IntMap a -> Maybe ((Int, a), M.IntMap a))
{-# INLINE minViewWithKey #-}

maxViewWithKey :: forall a. MonoidalIntMap a -> Maybe ((Int, a), MonoidalIntMap a)
maxViewWithKey = coerce (M.maxViewWithKey :: M.IntMap a -> Maybe ((Int, a), M.IntMap a))
{-# INLINE maxViewWithKey #-}

