{-|
Module      : Main
Description : The entry-point to KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module Main
  ( -- * The entry-point to KMonad
    main
  )
where

import K (begin)

main :: IO ()
main = begin

-- import qualified KMonad.App as KMonad (main)

-- import System.IO
-- import KMonad.Prelude
-- -- import KMonad.App.Cfg.Invoc (getInvoc)
-- import KMonad.App.Cfg.InvocNew
-- import KMonad.App.Cfg.Cfgable
-- import KMonad.App.Cfg.Default

-- -- main :: IO ()
-- -- main = KMonad.main

-- main :: IO ()
-- main = do
--   ivk <- getInvoc
--   pPrint $ ivk^.cfgChange.notes
--   pPrint $ runChange (ivk^.cfgChange.change) defAppCfg
