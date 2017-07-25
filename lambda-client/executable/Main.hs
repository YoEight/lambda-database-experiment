-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

import Control.Concurrent
import Control.Monad

import Lambda.Client

main :: IO ()
main = do
  _ <- newClientWithDefault
  forever $ do
    threadDelay 1000000
