{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module App where

import           Protolude               hiding ( hPutStrLn )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Control.Exception              ( finally )
import           System.Environment             ( lookupEnv )
import           Data.Maybe                     ( maybe )
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
import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Database.PostgreSQL.Typed      ( defaultPGDatabase
                                                , pgConnect
                                                , pgDisconnect
                                                )
import           Servant.Server                 ( serve )
import qualified Config


-- APP


run :: IO ()
run = do
  envPort      <- lookupEnv "PORT"
  pgConnection <- pgConnect Config.pgConfig

  let parsedPort = envPort >>= \p -> readMaybe p :: Maybe Int

  let port     = fromMaybe Config.defaultPort parsedPort

      settings = setPort port $ setBeforeMainLoop
        (hPutStrLn stderr ("listening on port " <> show port))
        defaultSettings

      policy = simpleCorsResourcePolicy
        { corsOrigins        = Just ([Config.feHost], False)
        , corsMethods        = Config.corsMethods
        , corsRequestHeaders = ["content-type"]
        }

  let application =
        cors (const $ Just policy) $ serve todoApi $ server pgConnection


  runSettings settings application `finally` pgDisconnect pgConnection
