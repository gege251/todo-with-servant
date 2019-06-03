{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Model.Todo where

import           Protolude
import           Data.UUID.V1                   ( nextUUID )
import qualified Data.UUID                     as UUID
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

import           Elm.Derive                     ( defaultOptions
                                                , deriveElmDef
                                                )

-- TODO

data Todo = Todo
  { id :: Text
  , value :: Text
  , done :: Bool
  } deriving (Generic)

deriveElmDef defaultOptions ''Todo

instance FromJSON Todo
instance ToJSON Todo



newtype NewTodo = NewTodo
  { value :: Text } deriving (Generic)

deriveElmDef defaultOptions ''NewTodo

instance FromJSON NewTodo
instance ToJSON NewTodo



createTodo :: NewTodo -> IO (Maybe Todo)
createTodo (NewTodo val) = do
  uuid <- nextUUID
  let uuidText = fmap UUID.toText uuid

  pure $ Todo <$> uuidText <*> Just val <*> Just False
