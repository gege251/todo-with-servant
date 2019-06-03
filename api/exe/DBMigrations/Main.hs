module Main where

import           Protolude
import           System.Environment             ( lookupEnv )
import           Database.PostgreSQL.Typed.Query
                                                ( PGSimpleQuery )
import           Database.PostgreSQL.Typed      ( PGConnection
                                                , pgSQL
                                                , pgExecute
                                                , pgConnect
                                                , pgDisconnect
                                                )
import           Config                         ( DBConfig(..)
                                                , toPGConfig
                                                )
import           Control.Exception              ( finally )


createTodoTable :: PGSimpleQuery ()
createTodoTable =
  [pgSQL|
    CREATE TABLE todo
        ( id text NOT NULL
        , value text NOT NULL
        , done boolean NOT NULL
        , PRIMARY KEY (id)
        )
  |]


printResult :: Text -> Int -> IO ()
printResult entity succeeded =
  putStrLn $ entity <> " created: " <> show succeeded


runMigrations :: PGConnection -> IO ()
runMigrations pgConnection = do
  todoTableCreated <- pgExecute pgConnection createTodoTable
  printResult "Todo table" todoTableCreated


main :: IO ()
main = do
  envDBHost <- lookupEnv "TPG_HOST"
  envDBName <- lookupEnv "TPG_DB"
  envDBUser <- lookupEnv "TPG_USER"
  envDBPass <- lookupEnv "TPG_PASS"

  let pgConfig = toPGConfig $ DBConfig envDBHost envDBName envDBUser envDBPass
  pgConnection <- pgConnect pgConfig

  runMigrations pgConnection `finally` pgDisconnect pgConnection
