--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Monitoring
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Monitoring
  ( Monitoring
  , createMonitoring
  , counterIncr
  , counterAdd
  , gaugeIncr
  , gaugeDecr
  , gaugeAdd
  , gaugeSubtract
  , gaugeSet
  , labelSet
  , labelModify
  , distributionAdd
  , distributionAddN
  ) where

--------------------------------------------------------------------------------
import qualified System.Metrics              as Metrics
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Gauge        as Gauge
import qualified System.Metrics.Label        as Label
import qualified System.Remote.Monitoring    as Monitoring

--------------------------------------------------------------------------------
import Lambda.Node.Prelude

--------------------------------------------------------------------------------
type Counts        = HashMap Text Counter.Counter
type Gauges        = HashMap Text Gauge.Gauge
type Labels        = HashMap Text Label.Label
type Distributions = HashMap Text Distribution.Distribution

--------------------------------------------------------------------------------
data Monitoring =
    Monitoring { _server :: Monitoring.Server
               , _counts :: MVar Counts
               , _gauges :: MVar Gauges
               , _labels :: MVar Labels
               , _dists  :: MVar Distributions
               }

--------------------------------------------------------------------------------
_store :: Monitoring -> Metrics.Store
_store = Monitoring.serverMetricStore . _server

--------------------------------------------------------------------------------
createMonitoring :: MonadBase IO m => m Monitoring
createMonitoring =
  Monitoring <$> liftBase (Monitoring.forkServer "0.0.0.0" 9000)
             <*> newMVar mempty
             <*> newMVar mempty
             <*> newMVar mempty
             <*> newMVar mempty

--------------------------------------------------------------------------------
lookupVarDefault :: MonadBaseControl IO m
                 => MVar (HashMap Text a)
                 -> Text
                 -> m a
                 -> m a
lookupVarDefault var name def =
  modifyMVar var $ \m ->
    case lookup name m of
      Nothing -> do
        a <- def
        return (insertMap name a m, a)
      Just a -> return (m, a)

--------------------------------------------------------------------------------
withMetric :: MonadBaseControl IO m
           => Monitoring
           -> (Monitoring -> MVar (HashMap Text a))
           -> (Metrics.Store -> IO a)
           -> Text
           -> m a
withMetric m getVar getDef name =
    liftBase $ lookupVarDefault (getVar m) name (getDef $ _store m)

--------------------------------------------------------------------------------
counterIncr :: MonadBaseControl IO m => Monitoring -> Text -> m ()
counterIncr m name = liftBase . Counter.inc =<< getCounter m name

--------------------------------------------------------------------------------
getCounter :: MonadBaseControl IO m
           => Monitoring
           -> Text
           -> m Counter.Counter
getCounter m name =
  withMetric m _counts (Metrics.createCounter name) name

--------------------------------------------------------------------------------
getGauge :: MonadBaseControl IO m
         => Monitoring
         -> Text
         -> m Gauge.Gauge
getGauge m name =
  withMetric m _gauges (Metrics.createGauge name) name

--------------------------------------------------------------------------------
getLabel :: MonadBaseControl IO m
         => Monitoring
         -> Text
         -> m Label.Label
getLabel m name =
  withMetric m _labels (Metrics.createLabel name) name

--------------------------------------------------------------------------------
getDistribution :: MonadBaseControl IO m
                => Monitoring
                -> Text
                -> m Distribution.Distribution
getDistribution m name =
  withMetric m _dists (Metrics.createDistribution name) name

--------------------------------------------------------------------------------
counterAdd :: MonadBaseControl IO m => Monitoring -> Text -> Int64 -> m ()
counterAdd m name value = do
  c <- getCounter m name
  liftBase $ Counter.add c value

--------------------------------------------------------------------------------
gaugeIncr :: MonadBaseControl IO m => Monitoring -> Text -> m ()
gaugeIncr m name = liftBase . Gauge.inc =<< getGauge m name

--------------------------------------------------------------------------------
gaugeDecr :: MonadBaseControl IO m => Monitoring -> Text -> m ()
gaugeDecr m name = liftBase . Gauge.dec =<< getGauge m name

--------------------------------------------------------------------------------
gaugeAdd :: MonadBaseControl IO m => Monitoring -> Text -> Int64 -> m ()
gaugeAdd m name value = do
  g <- getGauge m name
  liftBase $ Gauge.add g value

--------------------------------------------------------------------------------
gaugeSubtract :: MonadBaseControl IO m => Monitoring -> Text -> Int64 -> m ()
gaugeSubtract m name value = do
  g <- getGauge m name
  liftBase $ Gauge.subtract g value

--------------------------------------------------------------------------------
gaugeSet :: MonadBaseControl IO m => Monitoring -> Text -> Int64 -> m ()
gaugeSet m name value = do
  g <- getGauge m name
  liftBase $ Gauge.set g value

--------------------------------------------------------------------------------
labelSet :: MonadBaseControl IO m => Monitoring -> Text -> Text -> m ()
labelSet m name value = do
  l <- getLabel m name
  liftBase $ Label.set l value

--------------------------------------------------------------------------------
labelModify :: MonadBaseControl IO m
            => Monitoring
            -> (Text -> Text)
            -> Text
            -> m ()
labelModify m f name = do
  l <- getLabel m name
  liftBase $ Label.modify f l

--------------------------------------------------------------------------------
distributionAdd :: MonadBaseControl IO m => Monitoring -> Text -> Double -> m ()
distributionAdd m name value = do
  d <- getDistribution m name
  liftBase $ Distribution.add d value

--------------------------------------------------------------------------------
distributionAddN :: MonadBaseControl IO m
                 => Monitoring
                 -> Text
                 -> Double
                 -> Int64
                 -> m ()
distributionAddN m name value times = do
  d <- getDistribution m name
  liftBase $ Distribution.addN d value times


