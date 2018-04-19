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
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Pretty.Simple
import Web.Cookie
import Web.JWT

--------
-- DB --
--------

$(share
  [ mkPersist sqlSettings {mpsGenerateLenses = False}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
  User
    name Text

    deriving Eq
    deriving Show
    deriving Typeable

  TwitterUser
    username Text
    user UserId

    UniqueTwitterUserUsername username
    UniqueTwitterUserUser user

    deriving Eq
    deriving Show
    deriving Typeable
    |]
 )

----------------------
-- Servant Handlers --
----------------------

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

type AfterLoginApi = "after-login" :> Header "Cookie" Cookies :> Get '[HTML] Text

type HomePageApi = "index.html" :> Get '[HTML] Text

type Api = AfterLoginApi :<|> HomePageApi

serverRoot :: SqlBackend -> ServerT Api Handler
serverRoot backend = afterLogin backend :<|> homePage backend

afterLogin :: SqlBackend -> Maybe Cookies -> Handler Text
afterLogin backend maybeCookies = do
  case maybeCookies of
    Nothing -> error "no cookies"
    Just cookies ->
      case lookup "jwt" cookies of
        Nothing -> error "no jwt cookie"
        Just jwt -> do
          pPrint jwt
          let textJWT = decodeUtf8 jwt
              secretKey = secret "foobarbaz"
          let maybeVerifiedJWT = decodeAndVerifySignature secretKey textJWT
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
                    Just username -> do
                      userKey <- flip runSqlConn backend $ do
                        userKey <- insert $ User "fred"
                        insert_ $ TwitterUser username userKey
                        pure userKey
                      let newClaims = insertMap "userid" (Number $ fromIntegral $ fromSqlKey userKey) jwtClaims
                          newClaimsSet = (claims verifiedJWT) { unregisteredClaims = newClaims }
                          newJWT = encodeUtf8 $ encodeSigned HS256 secretKey newClaimsSet
                          newSetCookie =
                            defaultSetCookie
                              { setCookieName = "jwt"
                              , setCookieValue = newJWT
                              , setCookiePath = Just "/"
                              , setCookieDomain = Just ".servant-and-login-with.com"
                              , setCookieHttpOnly = True
                              , setCookieSecure = True
                              }
                      let headers =
                            [ ("Set-Cookie", toStrict . toLazyByteString $ renderSetCookie newSetCookie)
                            , ("Location", "https://servant-and-login-with.com:8443/index.html")
                            ]
                      throwError $ err302 { errHeaders = headers }

homePage :: SqlBackend -> Handler Text
homePage backend = do
  (twitterUsers, users) <- flip runSqlConn backend $ do
    (twitterUsers :: [Entity TwitterUser]) <- selectList [] []
    (users :: [Entity User]) <- selectList [] []
    pure (twitterUsers, users)
  putStrLn "twitter users:"
  pPrint twitterUsers
  putStrLn "users:"
  pPrint users
  pure "<p>hello world</p>"

----------
-- Main --
----------

instance MonadLogger IO where
  monadLoggerLog a b c d = runStdoutLoggingT $ monadLoggerLog a b c d

app :: SqlBackend -> Application
app = serveWithContext (Proxy @Api) EmptyContext . serverRoot

-- app :: Application
-- app req respFunc = do
--   pPrint req
--   respFunc $ responseLBS status200 [] "<p>hello</p>"

main :: IO ()
main =
  withSqliteConn ":memory:" $ \backend -> do
    runSqlConn (runMigration migrateAll) backend
    run 8000 $ app backend
