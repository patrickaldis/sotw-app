{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

-- (User (..), Song (..), Entry (..), SongRequest (..), migrateAll, SongId) where

import Data.Aeson (ToJSON)
import Database.Persist.TH
import GHC.Generics (Generic)

-- User table
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  name String
  age Int
  deriving Show Generic

Song
  spotID String
  title String
  artist String

Entry
  userID UserId
  songID SongId
|]

data SongRequest = SongRequest
  { spotID :: String,
    username :: String
  }
