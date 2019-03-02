{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module CodeGens.Elm where

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , intercalate
                                                )
import           Data.Monoid                    ( (<>) )
import           Elm                            ( toElmDecoderSource
                                                , toElmEncoderSource
                                                , toElmTypeSource
                                                )
import           Servant.Elm                    ( Proxy(Proxy)
                                                , ElmOptions
                                                , defElmOptions
                                                , defElmImports
                                                , UrlPrefix(..)
                                                , urlPrefix
                                                , generateElmForAPIWith
                                                )
import           Models.ApiModel                ( Todo
                                                , NewTodo
                                                )
import           Api                            ( TodoApi )


elmPath :: String
elmPath = "../client/src/Requests.elm"

options :: ElmOptions
options = defElmOptions { urlPrefix = Static "http://localhost:3030" }

elmHeader :: String -> Text
elmHeader moduleName =
  "module " <> pack moduleName <> " exposing (..)\n\n" <> defElmImports


elmfile :: [Text]
elmfile =
  elmHeader "Requests"
    : "type NoContent\n    = NoContent"
    : toElmTypeSource (Proxy :: Proxy Todo)
    : toElmTypeSource (Proxy :: Proxy NewTodo)
    : toElmDecoderSource (Proxy :: Proxy Todo)
    : toElmEncoderSource (Proxy :: Proxy Todo)
    : toElmEncoderSource (Proxy :: Proxy NewTodo)
    : generateElmForAPIWith options (Proxy :: Proxy TodoApi)



generate :: IO ()
generate = do
  putStrLn $ "Writing Elm queries functions to " ++ elmPath
  writeFile elmPath $ (unpack . intercalate (pack "\n\n\n")) elmfile
