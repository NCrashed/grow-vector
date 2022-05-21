{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Vector.Grow.Test where

import Control.Monad.Primitive (RealWorld)
import Data.Vector.Grow (GrowVector)
import qualified Data.Vector.Grow as G

import Test.Hspec

spec_creation :: Spec
spec_creation = describe "vector creation" $ do
  it "initialized with right size" $ do
    v :: GrowVector RealWorld Int <- G.new 42
    l <- G.length v
    l `shouldBe` 0
    c <- G.capacity v
    c `shouldBe` 42
    r <- G.null v
    r `shouldBe` True
  it "preallocated initialized with right size" $ do
    v :: GrowVector RealWorld Int <- G.newSized 23 42
    l <- G.length v
    l `shouldBe` 23
    c <- G.capacity v
    c `shouldBe` 42
    r <- G.null v
    r `shouldBe` False

spec_slicing :: Spec
spec_slicing = describe "vector slicing" $ do
  it "simple slice" $ do
    v :: GrowVector RealWorld Int <- G.thaw [1, 2, 3, 4, 5]
    v' <- G.slice 1 2 v
    vf <- G.freeze v'
    vf `shouldBe` [2, 3]
  it "empty slice" $ do
    v :: GrowVector RealWorld Int <- G.thaw [1, 2, 3, 4, 5]
    v' <- G.slice 1 0 v
    vf <- G.freeze v'
    vf `shouldBe` []
  it "begin slice" $ do
    v :: GrowVector RealWorld Int <- G.thaw [1, 2, 3, 4, 5]
    v' <- G.slice 0 1 v
    vf <- G.freeze v'
    vf `shouldBe` [1]
  it "end slice" $ do
    v :: GrowVector RealWorld Int <- G.thaw [1, 2, 3, 4, 5]
    v' <- G.slice 4 1 v
    vf <- G.freeze v'
    vf `shouldBe` [5]
  it "mutating parent slice" $ do
    v :: GrowVector RealWorld Int <- G.thaw [1, 2, 3, 4, 5]
    v' <- G.slice 1 2 v
    vf1 <- G.freeze v'
    vf1 `shouldBe` [2, 3]
    G.write v 2 4
    vf2 <- G.freeze v'
    vf2 `shouldBe` [2, 4]

spec_readWrite :: Spec
spec_readWrite = describe "basic read write" $ do
  it "read elements" $ do
    v :: GrowVector RealWorld Int <- G.thaw [1, 2, 3, 4, 5]
    a1 <- G.read v 0
    a1 `shouldBe` 1
    a2 <- G.read v 1
    a2 `shouldBe` 2
    a3 <- G.read v 2
    a3 `shouldBe` 3
    a4 <- G.read v 4
    a4 `shouldBe` 5
  it "writes elements" $ do
    v :: GrowVector RealWorld Int <- G.newSized 2 2
    G.write v 0 1
    G.write v 1 2
    v' <- G.freeze v
    v' `shouldBe` [1, 2]
  it "write-read indemponent" $ do
    v :: GrowVector RealWorld Int <- G.newSized 1 1
    G.write v 0 424242
    a <- G.read v 0
    a `shouldBe` 424242

spec_ensure :: Spec
spec_ensure = describe "capacity realloc" $ do
  it "ensure reallocates properly" $ do
    v :: GrowVector RealWorld Int <- G.new 5
    G.ensure v 1
    c1 <- G.capacity v
    c1 `shouldBe` 5
    G.ensure v 6
    c2 <- G.capacity v
    c2 `shouldBe` 6
  it "ensureAppend reallocates properly" $ do
    v :: GrowVector RealWorld Int <- G.new 5
    G.ensureAppend v 1
    c1 <- G.capacity v
    c1 `shouldBe` 5
    G.ensureAppend v 6
    c2 <- G.capacity v
    c2 `shouldBe` 8
    G.ensureAppend v 14
    c3 <- G.capacity v
    c3 `shouldBe` 17

spec_pushBack :: Spec
spec_pushBack = describe "push back operations" $ do
  it "push simple" $ do
    v :: GrowVector RealWorld Int <- G.new 1
    G.pushBack v 1
    l1 <- G.length v
    c1 <- G.capacity v
    l1 `shouldBe` 1
    c1 `shouldBe` 1
    G.pushBack v 2
    l2 <- G.length v
    c2 <- G.capacity v
    l2 `shouldBe` 2
    c2 `shouldBe` 3
    G.pushBack v 3
    l3 <- G.length v
    c3 <- G.capacity v
    l3 `shouldBe` 3
    c3 `shouldBe` 3
    G.pushBack v 4
    l4 <- G.length v
    c4 <- G.capacity v
    l4 `shouldBe` 4
    c4 `shouldBe` 5
    v' <- G.freeze v
    v' `shouldBe` [1, 2, 3, 4]
  it "push unsafe" $ do
    v :: GrowVector RealWorld Int <- G.new 1
    G.unsafePushBack v 1
    l1 <- G.length v
    c1 <- G.capacity v
    l1 `shouldBe` 1
    c1 `shouldBe` 1
    G.ensureAppend v 1
    G.unsafePushBack v 2
    l2 <- G.length v
    c2 <- G.capacity v
    l2 `shouldBe` 2
    c2 `shouldBe` 3
    G.unsafePushBack v 3
    l3 <- G.length v
    c3 <- G.capacity v
    l3 `shouldBe` 3
    c3 `shouldBe` 3
    G.ensureAppend v 1
    G.unsafePushBack v 4
    l4 <- G.length v
    c4 <- G.capacity v
    l4 `shouldBe` 4
    c4 `shouldBe` 5
    v' <- G.freeze v
    v' `shouldBe` [1, 2, 3, 4]
