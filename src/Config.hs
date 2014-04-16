{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative (empty, (<$>), (<*>))

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import Data.Text (Text())

import System.Directory (doesFileExist)

data SqliteConf = SqliteConf {
  sqliteFile :: Text
  } deriving (Eq, Show)

data AojConf = AojConf {
  user :: String,
  pass :: String
  } deriving (Eq, Show)

data Configuration = Configuration {
  port :: Int,
  aoj :: AojConf,
  sqlite :: Maybe SqliteConf
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    port = 16384,
    aoj = AojConf "" "",
    sqlite = Nothing
    }

instance AE.ToJSON SqliteConf where
  toJSON (SqliteConf file') =
    AE.object ["file" AE..= file']

instance AE.ToJSON AojConf where
  toJSON (AojConf user' pass') =
    AE.object ["user" AE..= user', "pass" AE..= pass']

instance AE.ToJSON Configuration where
  toJSON (Configuration port' aoj' sqlite') =
    AE.object (["port" AE..= port', "aoj" AE..= aoj'] ++ sqlite'')
    where sqlite'' = case sqlite' of
            Nothing -> []
            Just sqlite''' -> ["sqlite" AE..= sqlite''']

instance AE.FromJSON SqliteConf where
  parseJSON (AE.Object v) = SqliteConf <$>
                            v AE..: "file"
  parseJSON _ = empty

instance AE.FromJSON AojConf where
  parseJSON (AE.Object v) = AojConf <$>
                            v AE..: "user" <*>
                            v AE..: "pass"
  parseJSON _ = empty

instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) = Configuration <$>
                            v AE..: "port" <*>
                            v AE..: "aoj" <*>
                            v AE..:? "sqlite"
  parseJSON _ = empty

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig filepath = do
  existp <- doesFileExist filepath
  if existp then loadConfig' filepath else return Nothing
  where loadConfig' fp = do
          content <- BL.readFile fp
          return $ AE.decode content
