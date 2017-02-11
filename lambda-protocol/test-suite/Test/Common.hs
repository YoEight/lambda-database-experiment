--------------------------------------------------------------------------------
-- |
-- Module : Test.Common
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Common where

--------------------------------------------------------------------------------
import ClassyPrelude
import System.Directory

--------------------------------------------------------------------------------
freshFile :: FilePath -> IO FilePath
freshFile p = do
  let path = "trash/" ++ p
  createDirectoryIfMissing True "trash"
  whenM (doesFileExist path) $
    removeFile path

  return path
