--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Client.EndPoint
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.EndPoint where

--------------------------------------------------------------------------------
import Lambda.Prelude

--------------------------------------------------------------------------------
-- | Gathers both an IPv4 and a port.
data EndPoint =
    EndPoint
    { endPointIp   :: !String
    , endPointPort :: !Int
    } deriving Eq

--------------------------------------------------------------------------------
instance Show EndPoint where
    show (EndPoint h p) = h <> ":" <> show p

--------------------------------------------------------------------------------
emptyEndPoint :: EndPoint
emptyEndPoint = EndPoint "" 0

--------------------------------------------------------------------------------
data NodeEndPoints =
    NodeEndPoints
    { tcpEndPoint :: !EndPoint
    , secureEndPoint :: !(Maybe EndPoint)
    } deriving Show
