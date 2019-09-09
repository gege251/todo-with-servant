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
import           Elm.Derive                     ( defaultOptions
                                                , deriveBoth
                                                )

-- TODO

data Todo = Todo
  { id :: Text
  , value :: Text
  , done :: Bool
  } deriving (Generic)

deriveBoth defaultOptions ''Todo


data NewTodo = NewTodo
  { value :: Text } deriving (Generic)

deriveBoth defaultOptions ''NewTodo


createTodo :: NewTodo -> IO (Maybe Todo)
createTodo (NewTodo val) = do
  uuid <- nextUUID
  let uuidText = fmap UUID.toText uuid

  pure $ Todo <$> uuidText <*> Just val <*> Just False
