{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Config (
  ConfigApp(..)
, ConfigTls(..)
, ConfigServer(..)
, ConfigTelegram(..)
, loadConfig
, Opts(..)
, getOpts
)  
where

import GHC.Generics
import Data.Text
import qualified Dhall as Dh
import Options.Applicative
import Data.Semigroup ((<>))

data ConfigApp = ConfigApp {
  basePath  :: String
, useTls    :: Bool
, tls       :: ConfigTls
, server    :: ConfigServer
, telegram  :: ConfigTelegram
} deriving (Generic, Show)
instance Dh.Interpret ConfigApp

data ConfigTls = ConfigTls {
  crtFile :: String
, keyFile :: String
} deriving (Generic, Show)
instance Dh.Interpret ConfigTls

data ConfigServer = ConfigServer {
  port :: Dh.Natural
} deriving (Generic, Show)
instance Dh.Interpret ConfigServer

data ConfigTelegram = ConfigTelegram {
  token   :: String  
, chat_id :: Integer
} deriving (Generic, Show)
instance Dh.Interpret ConfigTelegram

loadConfig :: Text -> IO (ConfigApp)
loadConfig = Dh.input Dh.auto

newtype Opts = Opts { configFile :: Text }

getOpts :: IO Opts
getOpts = execParser opts
  where opts = info (opts' <**> helper)
          (fullDesc <> progDesc "Promote API server" <> header "Promote API server")
        opts' = Opts <$> pack <$> strOption (long "config" <> short 'c' <> help "Config file")
