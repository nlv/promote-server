{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Text
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.IO.Class

import System.IO

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import Network.Wai.Handler.WarpTLS
import qualified Network.HTTP.Types as HTTP

import qualified Config as Cfg
import Servant


data Callback = Callback
  { 
    callbackName  :: Text
  , callbackPhone :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Callback)

type API = "api" :> CallbackAPI
type CallbackAPI = "callback" :> ReqBody '[JSON] Callback :> Post '[JSON] () 

-- startApp :: IO ()
-- startApp = run 8080 app

app :: Application
-- app = serve api server
app = (cors corsPolicy) $ (serve api $ server)

api :: Proxy API
api = Proxy

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy _ = Just $ simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type"]
  , corsMethods = HTTP.methodPost : HTTP.methodOptions : HTTP.methodPut : corsMethods simpleCorsResourcePolicy
  }
  



server :: Server API
server = postCallback

startApp :: IO ()
startApp = do
  opts <- Cfg.getOpts
  config <- Cfg.loadConfig (Cfg.configFile opts)
  let serverCfg = Cfg.server config
  -- let url = pack $ Cfg.url serverCfg
  let port = fromIntegral (Cfg.port serverCfg)
  let crtFile = Cfg.crtFile $ Cfg.tls config
  let keyFile = Cfg.keyFile $ Cfg.tls config
      warpOpts =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
      tlsOpts = tlsSettings crtFile keyFile
  runTLS tlsOpts warpOpts =<< mkApp
  -- runServer warpOpts =<< mkApp


mkApp :: IO Application
mkApp = return $ (cors corsPolicy) $ static $ (serve api server)
  


postCallback :: Callback -> Handler ()
postCallback c = do
  liftIO $ putStrLn $ "post: " ++ show c
  return ()
