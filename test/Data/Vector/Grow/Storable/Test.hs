{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Vector.Grow.Storable.Test where

import Control.Monad.Primitive (RealWorld)
import Data.Vector.Grow.Storable (GrowVector)
import qualified Data.Vector.Grow.Storable as V

import Test.Tasty.Hspec

spec_creation :: Spec
spec_creation = describe "vector creation" $ do
  it "initialized with right size" $ do
    v :: GrowVector RealWorld Int <- V.new 42
    l <- V.length v
    l `shouldBe` 0
    c <- V.capacity v
    c `shouldBe` 42
    r <- V.null v
    r `shouldBe` True
  it "preallocated initialized with right size" $ do
    v :: GrowVector RealWorld Int <- V.newSized 23 42
    l <- V.length v
    l `shouldBe` 23
    c <- V.capacity v
    c `shouldBe` 42
    r <- V.null v
    r `shouldBe` False

spec_slicing :: Spec
spec_slicing = describe "vector slicing" $ do
  it "simple slice" $ do
    v :: GrowVector RealWorld Int <- V.thaw [1, 2, 3, 4, 5]
    v' <- V.slice 1 2 v
    vf <- V.freeze v'
    vf `shouldBe` [2, 3]
  it "empty slice" $ do
    v :: GrowVector RealWorld Int <- V.thaw [1, 2, 3, 4, 5]
    v' <- V.slice 1 0 v
    vf <- V.freeze v'
    vf `shouldBe` []
  it "begin slice" $ do
    v :: GrowVector RealWorld Int <- V.thaw [1, 2, 3, 4, 5]
    v' <- V.slice 0 1 v
    vf <- V.freeze v'
    vf `shouldBe` [1]
  it "end slice" $ do
    v :: GrowVector RealWorld Int <- V.thaw [1, 2, 3, 4, 5]
    v' <- V.slice 4 1 v
    vf <- V.freeze v'
    vf `shouldBe` [5]
  it "mutating parent slice" $ do
    v :: GrowVector RealWorld Int <- V.thaw [1, 2, 3, 4, 5]
    v' <- V.slice 1 2 v
    vf1 <- V.freeze v'
    vf1 `shouldBe` [2, 3]
    V.write v 2 4
    vf2 <- V.freeze v'
    vf2 `shouldBe` [2, 4]

spec_readWrite :: Spec
spec_readWrite = describe "basic read write" $ do
  it "read elements" $ do
    v :: GrowVector RealWorld Int <- V.thaw [1, 2, 3, 4, 5]
    a1 <- V.read v 0
    a1 `shouldBe` 1
    a2 <- V.read v 1
    a2 `shouldBe` 2
    a3 <- V.read v 2
    a3 `shouldBe` 3
    a4 <- V.read v 4
    a4 `shouldBe` 5
  it "writes elements" $ do
    v :: GrowVector RealWorld Int <- V.newSized 2 2
    V.write v 0 1
    V.write v 1 2
    v' <- V.freeze v
    v' `shouldBe` [1, 2]
  it "write-read indemponent" $ do
    v :: GrowVector RealWorld Int <- V.newSized 1 1
    V.write v 0 424242
    a <- V.read v 0
    a `shouldBe` 424242

spec_ensure :: Spec
spec_ensure = describe "capacity realloc" $ do
  it "ensure reallocates properly" $ do
    v :: GrowVector RealWorld Int <- V.new 5
    V.ensure v 1
    c1 <- V.capacity v
    c1 `shouldBe` 5
    V.ensure v 6
    c2 <- V.capacity v
    c2 `shouldBe` 6
  it "ensureAppend reallocates properly" $ do
    v :: GrowVector RealWorld Int <- V.new 5
    V.ensureAppend v 1
    c1 <- V.capacity v
    c1 `shouldBe` 5
    V.ensureAppend v 6
    c2 <- V.capacity v
    c2 `shouldBe` 8
    V.ensureAppend v 14
    c3 <- V.capacity v
    c3 `shouldBe` 17

spec_pushBack :: Spec
spec_pushBack = describe "push back operations" $ do
  it "push simple" $ do
    v :: GrowVector RealWorld Int <- V.new 1
    V.pushBack v 1
    l1 <- V.length v
    c1 <- V.capacity v
    l1 `shouldBe` 1
    c1 `shouldBe` 1
    V.pushBack v 2
    l2 <- V.length v
    c2 <- V.capacity v
    l2 `shouldBe` 2
    c2 `shouldBe` 3
    V.pushBack v 3
    l3 <- V.length v
    c3 <- V.capacity v
    l3 `shouldBe` 3
    c3 `shouldBe` 3
    V.pushBack v 4
    l4 <- V.length v
    c4 <- V.capacity v
    l4 `shouldBe` 4
    c4 `shouldBe` 5
    v' <- V.freeze v
    v' `shouldBe` [1, 2, 3, 4]
  it "push unsafe" $ do
    v :: GrowVector RealWorld Int <- V.new 1
    V.unsafePushBack v 1
    l1 <- V.length v
    c1 <- V.capacity v
    l1 `shouldBe` 1
    c1 `shouldBe` 1
    V.ensureAppend v 1
    V.unsafePushBack v 2
    l2 <- V.length v
    c2 <- V.capacity v
    l2 `shouldBe` 2
    c2 `shouldBe` 3
    V.unsafePushBack v 3
    l3 <- V.length v
    c3 <- V.capacity v
    l3 `shouldBe` 3
    c3 `shouldBe` 3
    V.ensureAppend v 1
    V.unsafePushBack v 4
    l4 <- V.length v
    c4 <- V.capacity v
    l4 `shouldBe` 4
    c4 `shouldBe` 5
    v' <- V.freeze v
    v' `shouldBe` [1, 2, 3, 4]
