{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    -- , app
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
import Network.HTTP.Base (urlEncode)
import Network.Curl

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

api :: Proxy API
api = Proxy

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy _ = Just $ simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type"]
  , corsMethods = HTTP.methodPost : HTTP.methodOptions : HTTP.methodPut : corsMethods simpleCorsResourcePolicy
  }

  
startApp :: IO ()
startApp = do
  opts <- Cfg.getOpts
  config <- Cfg.loadConfig (Cfg.configFile opts)
  let serverCfg = Cfg.server config
  let telegramCfg = Cfg.telegram config
  let telegramToken = Cfg.token telegramCfg
  let telegramChatId = fromIntegral $ Cfg.chat_id telegramCfg
  let port = fromIntegral (Cfg.port serverCfg)
  let crtFile = Cfg.crtFile $ Cfg.tls config
  let keyFile = Cfg.keyFile $ Cfg.tls config
  let useTls = Cfg.useTls config
  let basePath = Cfg.basePath config
      warpOpts =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
      tlsOpts = tlsSettings crtFile keyFile
  if useTls then runTLS tlsOpts warpOpts =<< mkApp basePath telegramToken telegramChatId
            else runSettings warpOpts =<< mkApp basePath telegramToken telegramChatId

appStatic :: String -> Middleware
appStatic basePath = 
  let route' = [("", "index.html"), ("smm", "promote-social.html")] 
      route = Prelude.map (\(a, b) -> (a, basePath ++ "/" ++ b)) route'
  in 
    staticPolicy $ only route <|> addBase basePath

mkApp :: String -> String -> Int -> IO Application
mkApp basePath tmToken tmChatId = 
  return $ (cors corsPolicy) $ appStatic basePath $ (serve api $ server tmToken tmChatId)
  
server :: String -> Int -> Server API
server = postCallback

postCallback :: String -> Int -> Callback -> Handler ()
postCallback tmToken tmChatId c = do
  let message = "Заявка. " 
         ++ " Имя: " ++ (unpack $ callbackName c)
         ++ " Телефон: " ++ (unpack $ callbackPhone c)
      url = "https://api.telegram.org/bot" 
         ++ tmToken 
         ++ "/sendMessage?chat_id="
         ++ (show tmChatId)
         ++ "parse_mode=html&text="
         ++ (urlEncode message)
  liftIO $ putStrLn $ "post: " ++ show c
  r <- liftIO $ curlGetResponse (url) []
  liftIO $ putStrLn $ "telegram resultcurlCode: " ++ (show $ respCurlCode r)
  liftIO $ putStrLn $ "telegram resultStatus: " ++ (show $ respStatus r)
  liftIO $ putStrLn $ "telegram resultStatusLine: " ++ (show $ respStatusLine r)
  return ()
