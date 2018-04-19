{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Pretty.Simple

app :: Application
app req respFunc = do
  pPrint req
  respFunc $ responseLBS status200 [] "<p>hello</p>"

main :: IO ()
main = run 8000 app
