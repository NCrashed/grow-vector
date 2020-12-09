{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Vector.Grow.Storable
-- Copyright   : (c) 2020 Gushcha Anton
-- License     : MIT
-- Maintainer  : ncrashed@protonmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Module defines mutable vector that can grow in size automatically when an user
-- adds new elements at the end of vector.
--
-- We reallocate vector with 1.5x length to get amortized append.
module Data.Vector.Grow.Storable(
    GrowVector(..)
  , IOGrowVector
  -- * Quering info about vector
  , length
  , null
  , capacity
  -- * Creation
  , new
  , newSized
  -- * Quering subvectors
  , slice
  -- * Converting to immutable
  , thaw
  , freeze
  -- * Capacity maninuplation
  , ensure
  , ensureAppend
  -- * Accessing individual elements
  , read
  , write
  , unsafeRead
  , unsafeWrite
  -- * Appending to vector
  , pushBack
  , unsafePushBack
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Storable.Mutable (MVector, Storable)
import GHC.Generics
import Prelude hiding (length, null, read)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

-- | Grow vector that is wrap around mutable vector. We allocate partially filled
-- vector and grow it when there is no more space for new element.
data GrowVector s a = GrowVector {
  growVector         :: !(MutVar s (MVector s a))
, growVectorLength   :: !(MutVar s Int)
} deriving (Generic)

-- | Synonim for 'GrowVector' in 'IO' monad
type IOGrowVector a = GrowVector RealWorld a

-- | Return current capacity of the vector (amount of elements that it can fit without realloc)
capacity :: (Storable a, PrimMonad m) => GrowVector (PrimState m) a -> m Int
capacity v = do
  mv <- readMutVar $ growVector v
  pure $ M.length mv
{-# INLINE capacity #-}

-- | Return current amount of elements in the vector
length :: (Storable a, PrimMonad m) => GrowVector (PrimState m) a -> m Int
length v = readMutVar $ growVectorLength v
{-# INLINE length #-}

-- | Return 'True' if there is no elements inside the vector
null :: (Storable a, PrimMonad m) => GrowVector (PrimState m) a -> m Bool
null v = fmap (== 0) $ length v
{-# INLINE null #-}

-- | Allocation of new growable vector with given capacity.
new :: (Storable a, PrimMonad m) => Int -> m (GrowVector (PrimState m) a)
new = newSized 0
{-# INLINE new #-}

-- | Allocation of new growable vector with given filled size and capacity.
-- Elements is not initialized. Capacity must be greater than filled size.
newSized :: (Storable a, PrimMonad m) => Int -> Int -> m (GrowVector (PrimState m) a)
newSized n cap = GrowVector <$> (newMutVar =<< M.new cap) <*> newMutVar n
{-# INLINABLE newSized #-}

-- | Yield a part of mutable vector without copying it. The vector must contain at least i+n elements.
slice :: (Storable a, PrimMonad m)
  => Int -- ^ i starting index
  -> Int -- ^ n number of elements
  -> GrowVector (PrimState m) a
  -> m (GrowVector (PrimState m) a)
slice i n v = do
  newSize <- newMutVar n
  mv <- readMutVar $ growVector v
  newVec <- newMutVar $! M.slice i n mv
  pure $! GrowVector newVec newSize
{-# INLINABLE slice #-}

-- | Convert immutable vector to grow mutable version. Doesn't allocate additonal memory for appending,
-- use 'ensure' to add capacity to the vector.
thaw :: (Storable a, PrimMonad m)
  => V.Vector a
  -> m (GrowVector (PrimState m) a)
thaw u = do
  mv <- newMutVar =<< V.thaw u
  lv <- newMutVar (V.length u)
  pure $ GrowVector mv lv
{-# INLINABLE thaw #-}

-- | Freezing growable vector. It will contain only actual elements of the vector not including capacity
-- space, but you should call 'V.force' on resulting vector to not hold the allocated capacity of original
-- vector in memory.

freeze :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> m (V.Vector a)
freeze v = do
  n <- length v
  mv <- readMutVar $ growVector v
  V.freeze $ M.take n mv
{-# INLINABLE freeze #-}

-- | Ensure that grow vector has at least given capacity possibly with reallocation.
ensure :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int
  -> m ()
ensure v n = do
  c <- capacity v
  unless (c >= n) $ do
    mv <- readMutVar $ growVector v
    writeMutVar (growVector v) =<< M.grow mv (n - c)
{-# INLINABLE ensure #-}

-- | Ensure that grow vector has enough space for additonal n elements.
-- We grow vector by 1.5 factor or by required elements count * 1.5.
ensureAppend :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Additional n elements
  -> m ()
ensureAppend v i = do
  n <- readMutVar $ growVectorLength v
  mv <- readMutVar $ growVector v
  let c = M.length mv
  unless (c >= n + i) $ do
    let growFactor = 1.5
        newCap = ceiling $ max (growFactor * fromIntegral c) (fromIntegral c + growFactor * fromIntegral (n + i - c))
    writeMutVar (growVector v) =<< M.grow mv (newCap - c)
{-# INLINABLE ensureAppend #-}

-- | Read element from vector at given index.
read :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> m a
read v i = do
  n <- readMutVar $ growVectorLength v
#ifndef LIQUID
  when (i < 0 || i >= n) $ error $ "GrowVector.read: index " <> show i <> " is out bounds " <> show n
#endif
  mv <- readMutVar $ growVector v
  M.unsafeRead mv i
{-# INLINABLE read #-}

-- | Read element from vector at given index.
unsafeRead :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> m a
unsafeRead v i = do
  mv <- readMutVar $ growVector v
  M.unsafeRead mv i
{-# INLINABLE unsafeRead #-}

-- | Write down element in the vector at given index.
write :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> a
  -> m ()
write v i a = do
  n <- readMutVar $ growVectorLength v
#ifndef LIQUID
  when (i < 0 || i >= n) $ error $ "GrowVector.write: index " <> show i <> " is out bounds " <> show n
#endif
  mv <- readMutVar $ growVector v
  M.unsafeWrite mv i a
{-# INLINABLE write #-}

-- | Write down element in the vector at given index.
unsafeWrite :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> a
  -> m ()
unsafeWrite v i a = do
  mv <- readMutVar $ growVector v
  M.unsafeWrite mv i a
{-# INLINABLE unsafeWrite #-}

-- | O(1) amortized appending to vector
pushBack :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> a
  -> m ()
pushBack v a = do
  ensureAppend v 1
  unsafePushBack v a
{-# INLINABLE pushBack #-}

-- | O(1) amortized appending to vector. Doesn't reallocate vector, so
-- there must by capacity - length >= 1.
unsafePushBack :: (Storable a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> a
  -> m ()
unsafePushBack v a = do
  n <- readMutVar $ growVectorLength v
  mv <- readMutVar $ growVector v
  M.write mv n a
  writeMutVar (growVectorLength v) (n+1)
{-# INLINABLE unsafePushBack #-}
