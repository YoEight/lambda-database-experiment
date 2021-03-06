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
import qualified Test.Serialize as Serialize

--------------------------------------------------------------------------------
main :: IO ()
main = do
    tree <- sequence [ testSpec "Serialize" Serialize.spec ]
    let test = Test.Tasty.testGroup "protocol" tree
    Test.Tasty.defaultMain test
