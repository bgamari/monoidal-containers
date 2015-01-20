{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides a 'Data.Map' variant which uses the value's
-- 'Monoid' instance to accumulate conflicting entries when merging
-- 'Map's.
--
-- While some functions mirroring those of 'Data.Map' are provided
-- here for convenience, more specialized needs will likely want to use
-- either the @Newtype@ or @Wrapped@ instances to manipulate the
-- underlying 'Map'.

module Data.Map.Monoidal
    ( MonoidalMap
    , singleton
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
newtype MonoidalMap k a = MM (M.Map k a)
    deriving (Show, Read, Functor, Eq, Ord, NFData,
              Foldable, Traversable,
              Data, Typeable)

type instance Index (MonoidalMap k a) = k
type instance IxValue (MonoidalMap k a) = a
instance Ord k => Ixed (MonoidalMap k a) where
    ix k f (MM m) = case M.lookup k m of
      Just v  -> f v <&> \v' -> MM (M.insert k v' m)
      Nothing -> pure (MM m)
    {-# INLINE ix #-}

instance Ord k => At (MonoidalMap k a) where
    at k f (MM m) = f mv <&> \r -> case r of
      Nothing -> maybe (MM m) (const (MM $ M.delete k m)) mv
      Just v' -> MM $ M.insert k v' m
      where mv = M.lookup k m
    {-# INLINE at #-}

instance Each (MonoidalMap k a) (MonoidalMap k b) a b

instance Ord k => FunctorWithIndex k (MonoidalMap k)
instance Ord k => FoldableWithIndex k (MonoidalMap k)
instance Ord k => TraversableWithIndex k (MonoidalMap k) where
    itraverse f (MM m) = fmap MM $ itraverse f m
    {-# INLINE itraverse #-}

instance Ord k => TraverseMin k (MonoidalMap k) where
    traverseMin f (MM m) = fmap MM $ traverseMin f m
    {-# INLINE traverseMin #-}
instance Ord k => TraverseMax k (MonoidalMap k) where
    traverseMax f (MM m) = fmap MM $ traverseMax f m
    {-# INLINE traverseMax #-}

instance AsEmpty (MonoidalMap k a) where
    _Empty = nearly (MM M.empty) (M.null . unpack)
    {-# INLINE _Empty #-}

instance Wrapped (MonoidalMap k a) where
    type Unwrapped (MonoidalMap k a) = M.Map k a
    _Wrapped' = iso unpack pack
    {-# INLINE _Wrapped' #-}

instance (Ord k, Monoid a) => Monoid (MonoidalMap k a) where
    mempty = MM mempty
    {-# INLINE mempty #-}
    MM a `mappend` MM b = MM $ M.unionWith mappend a b
    {-# INLINE mappend #-}

instance Newtype (MonoidalMap k a) (M.Map k a) where
    pack = MM
    {-# INLINE pack #-}
    unpack (MM a) = a
    {-# INLINE unpack #-}

#if MIN_VERSION_base(4,7,0)
instance Ord k => IsList (MonoidalMap k a) where
    type Item (MonoidalMap k a) = (k, a)
    fromList = MM . M.fromList
    {-# INLINE fromList #-}
    toList = M.toList . unpack
    {-# INLINE toList #-}
#endif

-- | /O(1)/. A map with a single element.
singleton :: Ord k => k -> a -> MonoidalMap k a
singleton k a = MM $ M.singleton k a
{-# INLINE singleton #-}
