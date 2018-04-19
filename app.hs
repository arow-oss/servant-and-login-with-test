{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import ClassyPrelude hiding (Handler)

import Data.Void
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Pretty.Simple

type AfterLoginApi = "after-login" :> Get '[HTML] Text

type HomePageApi = "index.html" :> Get '[HTML] Text

type Api = AfterLoginApi :<|> HomePageApi

serverRoot :: ServerT Api Handler
serverRoot = afterLogin :<|> homePage

afterLogin :: Handler Text
afterLogin = undefined

homePage :: Handler Text
homePage = pure "<p>hello world</p>"

app :: Application
app = serveWithContext (Proxy @Api) EmptyContext serverRoot

-- app :: Application
-- app req respFunc = do
--   pPrint req
--   respFunc $ responseLBS status200 [] "<p>hello</p>"

main :: IO ()
main = run 8000 app
