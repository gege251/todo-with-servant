module TodoActions where

import           Protolude
import           Database.PostgreSQL.Typed.Query ( PGSimpleQuery  )
import           Database.PostgreSQL.Typed      ( pgSQL  )

import           Servant                        ( NoContent(NoContent)
                                                , Handler
                                                )

import           Model.Todo


type DBQuery a = PGSimpleQuery a -> IO [a]


type DBExec = PGSimpleQuery () -> IO Int


toTodo :: (Text, Text, Bool) -> Todo
toTodo (todoId, todoValue, todoDone) = Todo todoId todoValue todoDone


getTodos :: DBQuery (Text, Text, Bool) -> Maybe Bool -> Handler [Todo]
getTodos dbQuery maybeFilter = do
  let query = case maybeFilter of
        Nothing -> 
          [pgSQL|SELECT id, value, done FROM todo|]
        Just todoDone ->
          [pgSQL|SELECT id, value, done FROM todo WHERE done = ${todoDone}|]

  todos <- liftIO $ dbQuery query
  pure $ map toTodo todos


getTodoById :: DBQuery (Text, Text, Bool) -> Text -> Handler (Maybe Todo)
getTodoById dbQuery todoId = do
  todos <- liftIO $ dbQuery [pgSQL|SELECT id, value, done FROM todo WHERE id = ${todoId}|]
  pure $ (map toTodo . listToMaybe) todos


putTodo :: DBExec -> NewTodo -> Handler (Maybe Todo)
putTodo dbExec todoVal = do
  maybeNewTodo <- liftIO $ createTodo todoVal
  case maybeNewTodo of
    Nothing -> pure Nothing
    Just newTodo@(Todo todoId todoValue todoDone) -> do
        _ <- liftIO $ dbExec
            [pgSQL|
                INSERT INTO todo (id, value, done)
                VALUES (${todoId}, ${todoValue}, ${todoDone})
            |]
        pure $ Just newTodo


delTodo :: DBExec -> Text -> Handler NoContent
delTodo dbExec todoId = do
  _ <- liftIO $ dbExec [pgSQL|DELETE FROM todo WHERE id = ${todoId}|]
  pure NoContent


updateTodo :: DBExec -> Text -> Todo -> Handler NoContent
updateTodo dbExec todoId (Todo _ todoValue todoDone) = do
  _ <- liftIO $ dbExec 
    [pgSQL|
		UPDATE todo
		SET value=${todoValue}, done=${todoDone}
		WHERE id = ${todoId}
	|]
  pure NoContent
