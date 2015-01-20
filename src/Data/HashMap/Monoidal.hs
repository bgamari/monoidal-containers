{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides a 'Data.HashMap' variant which uses the value's
-- 'Monoid' instance to accumulate conflicting entries when merging
-- 'Map's.
--
-- While some functions mirroring those of 'Data.HashMap' are provided
-- here for convenience, more specialized needs will likely want to use
-- either the @Newtype@ or @Wrapped@ instances to manipulate the
-- underlying 'Map'.

module Data.HashMap.Monoidal
    ( MonoidalHashMap
      -- * Often-needed functions
    , singleton
    , size
    , member
    , notMember
    , lookup
    , elems
    , keys
    ) where

import Prelude hiding (lookup)
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
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Control.Lens
import Control.Newtype

-- | A 'HashMap' with monoidal accumulation
newtype MonoidalHashMap k a = MM (M.HashMap k a)
    deriving (Show, Read, Functor, Eq, NFData,
              Foldable, Traversable,
              Data, Typeable)

type instance Index (MonoidalHashMap k a) = k
type instance IxValue (MonoidalHashMap k a) = a
instance (Eq k, Hashable k) => Ixed (MonoidalHashMap k a) where
    ix k f (MM m) = case M.lookup k m of
      Just v  -> f v <&> \v' -> MM (M.insert k v' m)
      Nothing -> pure (MM m)
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (MonoidalHashMap k a) where
    at k f (MM m) = f mv <&> \r -> case r of
      Nothing -> maybe (MM m) (const (MM $ M.delete k m)) mv
      Just v' -> MM $ M.insert k v' m
      where mv = M.lookup k m
    {-# INLINE at #-}

instance Each (MonoidalHashMap k a) (MonoidalHashMap k b) a b

instance (Eq k, Hashable k) => FunctorWithIndex k (MonoidalHashMap k)
instance (Eq k, Hashable k) => FoldableWithIndex k (MonoidalHashMap k)
instance (Eq k, Hashable k) => TraversableWithIndex k (MonoidalHashMap k) where
    itraverse f (MM m) = fmap MM $ itraverse f m
    {-# INLINE itraverse #-}

instance AsEmpty (MonoidalHashMap k a) where
    _Empty = nearly (MM M.empty) (M.null . unpack)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalHashMap k a) where
    type Unwrapped (MonoidalHashMap k a) = M.HashMap k a
    _Wrapped' = iso unpack pack
    {-# INLINE _Wrapped' #-}

instance (Eq k, Hashable k, Monoid a) => Monoid (MonoidalHashMap k a) where
    mempty = MM mempty
    {-# INLINE mempty #-}
    MM a `mappend` MM b = MM $ M.unionWith mappend a b
    {-# INLINE mappend #-}

instance Newtype (MonoidalHashMap k a) (M.HashMap k a) where
    pack = MM
    {-# INLINE pack #-}
    unpack (MM a) = a
    {-# INLINE unpack #-}

#if MIN_VERSION_base(4,7,0)
instance (Eq k, Hashable k) => IsList (MonoidalHashMap k a) where
    type Item (MonoidalHashMap k a) = (k, a)
    fromList = MM . M.fromList
    {-# INLINE fromList #-}
    toList = M.toList . unpack
    {-# INLINE toList #-}
#endif

-- | /O(1)/. A map with a single element.
singleton :: (Eq k, Hashable k) => k -> a -> MonoidalHashMap k a
singleton k a = MM $ M.singleton k a
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

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: (Eq k, Hashable k) => k -> MonoidalHashMap k a -> MonoidalHashMap k a
delete k = _Wrapping' MM %~ M.delete k
{-# INLINE delete #-}

-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
elems :: MonoidalHashMap k a -> [a]
elems = M.elems . unpack
{-# INLINE elems #-}

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
keys :: MonoidalHashMap k a -> [k]
keys = M.keys . unpack
{-# INLINE keys #-}
