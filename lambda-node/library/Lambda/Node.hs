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
import           Lambda.Node.Bus
import qualified Lambda.Node.Manager.Connection as Connection
import qualified Lambda.Node.Manager.Timer      as Timer
import           Lambda.Node.Logger
import           Lambda.Node.Monitoring
import           Lambda.Node.Prelude
import           Lambda.Node.Settings
import           Lambda.Node.Types

--------------------------------------------------------------------------------
nodeMain :: IO ()
nodeMain = do
  setts <- parseArgs
  print setts

  let LoggingSettings{..} = loggingSettings setts
  runtime <- Runtime setts <$> newLoggerRef loggingType loggingLevel False
                           <*> createMonitoring

  mainBus <- newBus runtime "main-bus"

  Timer.new mainBus
  Connection.new mainBus runtime setts

  busProcessedEverything mainBus