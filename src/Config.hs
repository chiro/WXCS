{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative (empty, (<$>), (<*>))

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default

import System.Directory (doesFileExist)

data Configuration = Configuration {
  port :: Int,
  aojUser :: String,
  aojPass :: String,
  proxyHost :: Maybe String,
  proxyPort :: Maybe Int
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    port = 16384,
    aojUser = "",
    aojPass = "",
    proxyHost = Nothing,
    proxyPort = Nothing
    }

instance FromJSON Configuration where
  parseJSON (Object v) = Configuration <$>
                            v .: "port" <*>
                            v .: "aojUser" <*>
                            v .: "aojPass" <*>
                            v .:? "proxyHost" <*>
                            v .:? "proxyPort"
  parseJSON _ = empty

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig filepath = do
  existp <- doesFileExist filepath
  if existp then loadConfig' filepath else return Nothing
  where loadConfig' fp = do
          content <- BL.readFile fp
          return $ decode content
