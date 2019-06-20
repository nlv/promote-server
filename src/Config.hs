{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Config (
  ConfigApp(..)
, ConfigTls(..)
, ConfigServer(..)
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
  tls       :: ConfigTls
, server    :: ConfigServer
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

loadConfig :: Text -> IO (ConfigApp)
loadConfig = Dh.input Dh.auto

newtype Opts = Opts { configFile :: Text }

getOpts :: IO Opts
getOpts = execParser opts
  where opts = info (opts' <**> helper)
          (fullDesc <> progDesc "Promote API server" <> header "Promote API server")
        opts' = Opts <$> pack <$> strOption (long "config" <> short 'c' <> help "Config file")
