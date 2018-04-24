{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DbHelpers where

import ClassyPrelude

import Database.Persist.Sqlite

newtype TwitterId = TwitterId Text
  deriving newtype (PersistField, PersistFieldSql, Show)
  deriving stock Eq

