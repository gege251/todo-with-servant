{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts  #-}

module TodoActions where

import           Protolude
import           GHC.Int                        ( Int64 )
import           Data.Maybe                     ( maybeToList )

import           Database.PostgreSQL.Typed.Query
                                                ( PGQuery )
import           Database.PostgreSQL.Typed      ( pgSQL )

import           Servant                        ( NoContent(NoContent)
                                                , Handler
                                                )

import qualified Models.ApiModel               as Api


type DBQuery q a = PGQuery q a => q -> IO a

-- type DBExec q = PGQuery q () => q -> IO Int

toTodo :: (Text, Text, Bool) -> Api.Todo
toTodo (id, value, done) = Api.Todo id value done


-- getTodos :: DBQuery (Text, Text, Bool) -> Maybe Bool -> Handler [Api.Todo]
getTodos dbQuery maybeFilter = do
  -- let filters = (==.) Api.TodoDone <$> maybeToList maybeFilter
  todos <- liftIO $ dbQuery [pgSQL|SELECT id, value, done FROM todo |]
  pure $ map toTodo todos


-- getTodoById :: DBQuery -> Int64 -> Handler (Maybe Api.Todo)
-- getTodoById dbQuery todoId = do
--   todo <- runDB $ selectFirst [Api.TodoId ==. (toSqlKey todoId)] []
--   return $ fmap toApiModel todo


-- putTodo :: DBExec -> Api.NewTodo -> Handler Api.Todo
-- putTodo dbExec todoVal = do
--   let newTodo = Api.Todo (Api.value (todoVal :: Api.NewTodo)) False
--   todoKey <- runDB $ insert newTodo
--   return
--     $ Api.Todo (fromSqlKey todoKey) (Api.value (todoVal :: Api.NewTodo)) False


-- delTodo :: DBExec -> Int64 -> Handler NoContent
-- delTodo dbExec todoId = do
--   runDB $ deleteWhere [Api.TodoId ==. (toSqlKey todoId)]
--   return NoContent


-- updateTodo :: DBExec -> Int64 -> Api.Todo -> Handler NoContent
-- updateTodo dbExec todoId todo = do
--   let dbTodo = Api.Todo (Api.value (todo :: Api.Todo)) (Api.done todo)
--   runDB $ replace (toSqlKey todoId :: Api.TodoId) dbTodo
--   return NoContent
