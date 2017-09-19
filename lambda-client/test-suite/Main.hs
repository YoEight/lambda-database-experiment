--------------------------------------------------------------------------------
-- |
-- Module    :  Main
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Lambda.Client

main :: IO ()
main = do
    -- test <- testSpec "lambda-client" spec
    -- Test.Tasty.defaultMain test
  client <- newClientWithDefault
  awaitShutdown client

  return ()

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
