--------------------------------------------------------------------------------
-- |
-- Module : Test.TransactionLog
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.TransactionLog (spec) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID.V4
import System.Directory
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import Server.Messaging
import Server.TransactionLog

--------------------------------------------------------------------------------
freshFilePath :: IO FilePath
freshFilePath = do
  createDirectoryIfMissing True "trash"
  pid <- nextRandom
  return $ "trash/" ++ show pid ++ ".test"

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "create new" $ do
    path   <- freshFilePath
    (_, p) <- newExchange
    _      <- newBackend path p

    return ()
