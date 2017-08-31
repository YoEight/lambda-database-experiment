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
module Main where

--------------------------------------------------------------------------------
import Lambda.Node
import Lambda.Prelude

--------------------------------------------------------------------------------
main :: IO ()
main = Lambda.Node.nodeMain
