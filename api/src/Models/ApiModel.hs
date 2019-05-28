{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module Models.ApiModel where

import           Protolude
import           GHC.Int                        ( Int64 )
import           Data.UUID.V1                   ( nextUUID )
import qualified Data.UUID                     as UUID
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Servant.Elm                    ( ElmType )


-- TODO

data Todo = Todo
  { id :: Text
  , value :: Text
  , done :: Bool
  } deriving (Eq, Show, Generic)


instance FromJSON Todo
instance ToJSON Todo
instance ElmType Todo


newtype NewTodo = NewTodo
  { value :: Text } deriving Generic

instance FromJSON NewTodo
instance ToJSON NewTodo
instance ElmType NewTodo


createTodo :: NewTodo -> IO (Maybe Todo)
createTodo (NewTodo val) = do
  uuid <- nextUUID
  let uuidText = fmap UUID.toText uuid

  pure $ Todo <$> uuidText <*> Just val <*> Just False
