{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Test.Bus
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Bus (spec) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import Server.Bus

--------------------------------------------------------------------------------
data Foo = Foo deriving Show
data Bar = Bar deriving Show

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "subcribe-publish" $ do
    ref <- newIORef (0 :: Int)
    bus <- newBus

    subscribe bus $ \Foo -> do
      atomicModifyIORef' ref $ \i ->
        (i + 1, ())

    subscribe bus $ \Bar ->
      atomicModifyIORef' ref $ \i ->
        (i + 2, ())

    publish bus Foo
    publish bus Bar

    i <- readIORef ref

    i `shouldBe` 3

  specify "subcribe-publish-message" $ do
    ref <- newIORef (0 :: Int)
    bus <- newBus

    subscribe bus $ \Foo -> do
      atomicModifyIORef' ref $ \i ->
        (i + 1, ())

    subscribe bus $ \msg -> do
      let res = fromMsg msg
      case res of
        Just (_ :: Foo) ->
          atomicModifyIORef' ref $ \i ->
            (i + 2, ())

        _ -> return ()

    publish bus Foo
    publish bus Bar

    i <- readIORef ref

    i `shouldBe` 3
