{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.Lock (new)
import Control.Monad.Reader

import Data.Maybe (fromJust, isNothing)

import qualified Database.Persist.Sqlite as Sq

import Web.Scotty.Trans (scottyT)

import App
import Config
import Model (migrateAll)
import Submit
import Utils

main :: IO ()
main = do
  config' <- loadConfig "wxcs.conf"
  when (isNothing config') $ error "Config file (wxcs.conf) not found"
  let config = fromJust config'

  case sqlite config of
    Nothing -> error "Error: Database is not found!"
    Just sqliteConf -> do
      let dbFile = sqliteFile sqliteConf
      Sq.runSqlite dbFile $ Sq.runMigration migrateAll
  -- TODO: error handling?
  lock <- new
  forkIO_ $ runReaderT crawler (lock, config)
  scottyT (port config) ((flip runReaderT) (lock, config))
    ((flip runReaderT) (lock, config)) app
