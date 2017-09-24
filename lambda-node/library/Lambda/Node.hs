--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node (nodeMain) where

--------------------------------------------------------------------------------
import           Lambda.Bus
import qualified Lambda.Node.Manager.Connection as Connection
import           Lambda.Prelude

--------------------------------------------------------------------------------
nodeMain :: IO ()
nodeMain = lambdaMain $ do
  mainBus <- newBus
  Connection.new mainBus
  busProcessedEverything mainBus
