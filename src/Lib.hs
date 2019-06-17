{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Data.Text

data Callback = Callback
  { 
    callbackName  :: Text
  , callbackPhone :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Callback)

type API = "callback" :> ReqBody '[JSON] Callback :> Post '[JSON] () 

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = postCallback

postCallback :: Callback -> Handler ()
postCallback _ = return ()
