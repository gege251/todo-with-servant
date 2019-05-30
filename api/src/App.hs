module App where

import           Protolude               hiding ( hPutStrLn )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Control.Exception              ( finally )
import           System.Environment             ( lookupEnv )
import           Network.Wai.Middleware.Cors    ( simpleCorsResourcePolicy
                                                , cors
                                                , corsMethods
                                                , corsOrigins
                                                , corsRequestHeaders
                                                )

import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , setPort
                                                , setBeforeMainLoop
                                                , runSettings
                                                )


import           Api                            ( todoApi
                                                , server
                                                )
import           Database.PostgreSQL.Typed      ( pgConnect
                                                , pgDisconnect
                                                )
import           Servant.Server                 ( serve )
import           Config                         ( DBConfig(..)
                                                , toPGConfig
                                                )
import qualified Config


-- APP


run :: IO ()
run = do
  envPort   <- lookupEnv "PORT"
  envDBHost <- lookupEnv "TPG_HOST"
  envDBName <- lookupEnv "TPG_DB"
  envDBUser <- lookupEnv "TPG_USER"
  envDBPass <- lookupEnv "TPG_PASS"

  let parsedPort = envPort >>= \p -> readMaybe p :: Maybe Int
  let pgConfig   = toPGConfig $ DBConfig envDBHost envDBName envDBUser envDBPass

  let port       = fromMaybe Config.defaultPort parsedPort


  let settings = setPort port $ setBeforeMainLoop
        (hPutStrLn stderr ("listening on port " <> show port))
        defaultSettings

  let policy = simpleCorsResourcePolicy
        { corsOrigins        = Just ([Config.feHost], False)
        , corsMethods        = Config.corsMethods
        , corsRequestHeaders = ["content-type"]
        }

  pgConnection <- pgConnect pgConfig

  let application =
        cors (const $ Just policy) $ serve todoApi $ server pgConnection

  runSettings settings application `finally` pgDisconnect pgConnection
