-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

import ClassyPrelude
import LDE.Client
import LDE.Internal.Settings

main :: IO ()
main = do
  _ <- newClient (Static "127.0.0.1" 1113)
  forever $ do
    threadDelay 1000000
