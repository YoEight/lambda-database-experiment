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
import Control.Concurrent.Async
import Data.List.NonEmpty
import Data.Aeson
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Lambda.Client
import ClassyPrelude hiding (fromList)

main :: IO ()
main = do
    -- test <- testSpec "lambda-client" spec
    -- Test.Tasty.defaultMain test
  client <- newClientWithDefault
  eid <- freshId
  let payload = object [ "IsHaskellTheBest" .= True ]
      evt     = Event { eventType     = "lde-mockup"
                      , eventId       = eid
                      , eventPayload  = Data $ toStrict $ encode $ payload
                      , eventMetadata = Nothing
                      }

  wres <- writeEvents client "test-stream" (fromList [evt]) AnyVersion >>= wait
  print wres

  rres <- readEvents client "test-stream" (startFrom 1) >>= wait
  print wres

  awaitShutdown client

  return ()

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
