{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import ClassyPrelude hiding (Handler)

import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Data.Aeson (Value(Number))
import Data.Aeson.Lens
import Data.ByteString.Builder
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.HTTP.Types.Header (hCookie, hSetCookie)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Servant.Server.Experimental.Auth
import Text.Pretty.Simple
import Web.Cookie
import Web.JWT

import DbHelpers (TwitterId(TwitterId))

--------
-- DB --
--------

$(share
  [ mkPersist sqlSettings {mpsGenerateLenses = False}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
  User
    twitterId TwitterId
    name      Text

    UniqueUserTwitterId twitterId

    deriving Eq
    deriving Show

  -- This simply a log of all the times a user has logged in.
  -- It is shown on the homepage.
  LoginLog
    userId TwitterId
    time  UTCTime

    deriving Eq
    deriving Show
    |]
 )

----------------------
-- Servant Handlers --
----------------------

jwtSecret :: Secret
jwtSecret = secret "foobarbaz"

instance FromHttpApiData Cookies where
  parseHeader = Right . parseCookies

instance MonadUnliftIO Handler where
  askUnliftIO :: Handler (UnliftIO Handler)
  -- XXX: Throw away errors:
  askUnliftIO =
    pure $
      UnliftIO
        (\(Handler (ExceptT ioEither)) ->
            ioEither >>= either (const (error "throw away servant err")) pure
        )

type AfterLoginApi = "after-login" :> AuthProtect "jwt" :> Get '[HTML] Text

type HomePageApi = "index.html" :> AuthProtect "jwt" :> Get '[HTML] Text

type Api = AfterLoginApi :<|> HomePageApi

serverRoot :: SqlBackend -> ServerT Api Handler
serverRoot backend = afterLogin backend :<|> homePage backend

afterLogin :: SqlBackend -> TwitterId -> Handler Text
afterLogin backend twitterId = do
  time <- liftIO getCurrentTime
  flip runSqlConn backend $ do
    maybeUser <- getBy $ UniqueUserTwitterId twitterId
    case maybeUser of
      Nothing -> do
        -- create new user
        insert_ $ User twitterId "fred"
      Just (Entity _ _) ->
        -- don't do anything if the user already exists
        pure ()
    insert_ $ LoginLog twitterId time
  let headers =
        [ ( "Location"
          , "https://servant-and-login-with.com:8443/index.html"
          )
        ]
  throwError $ err302 { errHeaders = headers }

homePage :: SqlBackend -> TwitterId -> Handler Text
homePage backend twitterId = do
  (users, logins) <- flip runSqlConn backend $ do
    users :: [Entity User] <- selectList [] []
    logins :: [Entity LoginLog] <- selectList [LoginLogUserId ==. twitterId] []
    pure (users, logins)
  putStrLn "users:"
  pPrint users
  putStrLn "logins:"
  pPrint logins
  pure $ "<p>authenticated as: " <> tshow twitterId <> "</p>"

type instance AuthServerData (AuthProtect "jwt") = TwitterId

auther :: AuthHandler Request (AuthServerData (AuthProtect "jwt"))
auther = mkAuthHandler $ \req -> do
  let headers = requestHeaders req
  case lookup "Cookie" headers of
    Nothing -> error "no cookies"
    Just rawCookies -> do
      let cookies = parseCookies rawCookies
      case lookup "jwt" cookies of
        Nothing -> error "no jwt cookie"
        Just jwt -> do
          pPrint jwt
          let textJWT = decodeUtf8 jwt
          let maybeVerifiedJWT = decodeAndVerifySignature jwtSecret textJWT
          putStrLn "maybeVerifiedJWT:"
          pPrint maybeVerifiedJWT
          case maybeVerifiedJWT of
            Nothing -> error "could not verify jwt"
            Just verifiedJWT -> do
              let jwtClaims = unregisteredClaims $ claims verifiedJWT
              case lookup "profile" jwtClaims of
                Nothing -> error "could not find the twitter profile in jwt claims"
                Just profileJSON ->
                  case profileJSON ^? key "username" . _String of
                    Nothing -> error "could not find the username key in the profile"
                    Just username -> pure $ TwitterId username

----------
-- Main --
----------

instance MonadLogger IO where
  monadLoggerLog a b c d = runStdoutLoggingT $ monadLoggerLog a b c d

app :: SqlBackend -> Application
app = serveWithContext (Proxy @Api) (auther :. EmptyContext) . serverRoot

refreshCookieMiddleware :: Middleware
refreshCookieMiddleware app req respFunc = app req go
  where
    go :: Response -> IO ResponseReceived
    go resp = do
      print "in middleware...."
      time <- getCurrentTime
      let respHeaders = responseHeaders resp
      case lookup hSetCookie respHeaders of
        Just _ -> respFunc resp
        Nothing ->
          case requestMethod req of
            "GET" -> do
              let reqHeaders = requestHeaders req
              case lookup hCookie reqHeaders of
                Nothing -> respFunc resp
                Just rawCookies -> do
                  let cookies = parseCookies rawCookies
                  case lookup "jwt" cookies of
                    Nothing -> respFunc resp
                    Just rawJwt -> do
                      let newCookie =
                            defaultSetCookie
                              { setCookieName = "jwt"
                              , setCookieValue = rawJwt
                              , setCookiePath = Just "/"
                              , setCookieDomain = Just ".servant-and-login-with.com"
                              , setCookieHttpOnly = True
                              , setCookieSecure = True
                              , setCookieMaxAge = Just $ 60 * 60 * 24 * 14 -- two weeks
                              }
                          newSetCookieHeader =
                            [ ("Set-Cookie", toStrict . toLazyByteString $ renderSetCookie newCookie) ]
                          newResp = mapResponseHeaders (<> newSetCookieHeader) resp
                      respFunc newResp
            _ -> respFunc resp

main :: IO ()
main =
  withSqliteConn ":memory:" $ \backend -> do
    runSqlConn (runMigration migrateAll) backend
    run 8000 . refreshCookieMiddleware $ app backend
