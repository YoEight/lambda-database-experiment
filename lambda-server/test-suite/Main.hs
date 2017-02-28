--------------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
import           ClassyPrelude
import qualified Test.Tasty
import           Test.Tasty.Hspec

--------------------------------------------------------------------------------
import qualified Test.TransactionLog as TLog
import qualified Test.Bus            as Bus

--------------------------------------------------------------------------------
main :: IO ()
main = do
    tree <- sequence [ testSpec "transaction-log" TLog.spec
                     , testSpec "bus" Bus.spec
                     ]

    let test = Test.Tasty.testGroup "lambda-server" tree
    Test.Tasty.defaultMain test
