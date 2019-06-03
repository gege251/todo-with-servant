module Api where

import           Protolude
import           Control.Lens                   ( (&)
                                                , (?~)
                                                , mapped
                                                )
import           Servant
import           Servant.Docs
import           Data.Aeson                     ( toJSON )
import           Data.Swagger                   ( ToSchema
                                                , declareNamedSchema
                                                , example
                                                , schema
                                                , description
                                                , defaultSchemaOptions
                                                , genericDeclareNamedSchema
                                                )
import           Database.PostgreSQL.Typed      ( PGConnection
                                                , pgQuery
                                                , pgExecute
                                                )

import           Model.Todo
import qualified TodoActions


-- API


type TodoApi =
  -- GET /todo Returns every todo
       "todo" :> QueryParam "done" Bool :> Get '[JSON] [Todo]

  -- GET /todo/:todoId Returns the todo with the given ID
  :<|> "todo" :> Capture "todoId" Text :> Get '[JSON] (Maybe Todo)

  -- POST /todo Creates a new todo
  :<|> "todo" :> ReqBody '[JSON] NewTodo :> PostCreated '[JSON] (Maybe Todo)

  -- DELETE /todo/:todoid Deletes todo with the given ID
  :<|> "todo" :> Capture "todoId" Text :> DeleteAccepted '[JSON] ()

  -- PUT /todo/:todoid Replaces todo of the given ID
  :<|> "todo" :> Capture "todoId" Text :> ReqBody '[JSON] Todo :> PutAccepted '[JSON] ()


todoApi :: Proxy TodoApi
todoApi = Proxy


server :: PGConnection -> ServerT TodoApi Handler
server pgConnection =
  let dbQuery = pgQuery pgConnection
      dbExec  = pgExecute pgConnection
  in  TodoActions.getTodos dbQuery
      :<|> TodoActions.getTodoById dbQuery
      :<|> TodoActions.putTodo dbExec
      :<|> TodoActions.delTodo dbExec
      :<|> TodoActions.updateTodo dbExec


-- DOCS

instance ToParam (QueryParam "done" Bool) where
  toParam _ = DocQueryParam "done" [] "Filter by done state" Normal

instance ToCapture (Capture "todoId" Text) where
  toCapture _ = DocCapture "todoId" "(string) Todo ID"

instance ToSample Int64 where
  toSamples _ = singleSample 1

instance ToSample () where
  toSamples _ = singleSample ()

instance ToSample Todo where
  toSamples _ = singleSample (Todo "uuid-1234" "buy me a beer" False)

instance ToSchema Todo where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & (mapped.schema.description ?~ "This is some real Todo right here")
        & (mapped.schema.example ?~ toJSON (Todo "uuid-1234" "buy me a beer" False))

instance ToSample NewTodo where
  toSamples _ = singleSample (NewTodo "buy me a beer")

instance ToSchema NewTodo where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & (mapped.schema.description ?~ "This is a new todo")
        & (mapped.schema.example ?~ toJSON (NewTodo "buy me a beer"))
