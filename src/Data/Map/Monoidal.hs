{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Map.Monoidal ( MonoidalMap ) where

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
    toList = M.toList . unpack
#endif
