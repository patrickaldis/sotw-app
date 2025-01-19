module Main where

import API (app)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql (createPostgresqlPool, insert, runMigration, runSqlConn, runSqlPool)
import Network.Wai.Handler.Warp (run)
import Schema (Song (..), User (..), migrateAll)

main :: IO ()
main = runStderrLoggingT $ do
  pool <-
    createPostgresqlPool
      ( pack . unwords $
          [ "host=localhost",
            "dbname=mydatabase",
            "user=myuser",
            "password=mypassword",
            "port=5432"
          ]
      )
      5
  runSqlPool (runMigration migrateAll) pool
  runSqlPool (insert $ Song "someid" "patricius" "magicius") pool
  runSqlPool (insert $ User "Patrick" 1) pool
  liftIO $ run 8080 $ app pool
