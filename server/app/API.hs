{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API (app) where

import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Control.Monad.Reader (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser, ToJSON (..))
import Database.Persist.Sql
import Database.Persist.TH
import Network.Wai.Handler.Warp (run)
import Schema
import Servant
import Servant.Server (Handler, Server)
import Text.Printf (printf)

type API =
  -- GET http://localhost:8080/users
  "users" :> Get '[JSON] [User]
    -- POST http://localhost:8080/users
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "addSong" :> ReqBody '[JSON] SongRequest :> Post '[JSON] Entry

server :: ConnectionPool -> Server API
server pool = getUsers :<|> postUser :<|> postSong
  where
    getUsers :: Handler [User]
    getUsers = liftIO $ runSqlPool (map entityVal <$> selectList [] []) pool

    postUser :: User -> Handler User
    postUser user = do
      liftIO $ runSqlPool (insert user) pool
      return user

    postSong :: SongRequest -> Handler Entry
    postSong (SongRequest sID uName) = liftIO $ do
      runSqlPool
        ( do
            song <-
              ( do
                  res <- selectFirst [SongSpotID ==. sID] []
                  case res of
                    Nothing -> liftIO $ throwIO $ ContentError "Not Implemented"
                    Just songEntity -> return songEntity
                )
            user <-
              ( do
                  res <- selectFirst [UserName ==. uName] []
                  case res of
                    Nothing -> liftIO $ throwIO $ ContentError $ unwords ["User", uName, "not found"]
                    Just userEntity -> return userEntity
                )
            let entry = Entry (entityKey user) (entityKey song)
            insert entry
            return entry
        )
        pool

data ContentError = ContentError String deriving (Show)

instance Exception ContentError

-- let song =
--       Song
--         { songTitle = "",
--           songArtist = "",
--           songSpotID = ""
--         }
--     entry = Entry _ _
-- runSqlPool (insert entry) pool
-- return entry

instance ToJSON User where
  toJSON user =
    object
      [ "name" .= userName user,
        "age" .= userAge user
      ]

instance ToJSON Entry where
  toJSON entry =
    object
      [ "userID" .= entryUserID entry,
        "songID" .= entrySongID entry
      ]

instance FromJSON User where
  parseJSON (Object a) = do
    name <- a .: "name"
    age <- a .: "age"
    return
      ( User
          { userName = name,
            userAge = age
          }
      )

instance FromJSON SongRequest where
  parseJSON (Object a) = do
    spotID <- a .: "spotID"
    uName <- a .: "uName"
    return (SongRequest spotID uName)

app :: ConnectionPool -> Application
app = serve (Proxy :: Proxy API) . server
