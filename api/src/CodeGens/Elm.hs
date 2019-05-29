module CodeGens.Elm where

import           Protolude               hiding ( intercalate )
import           Data.Text                      ( intercalate )
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
import           Model.Todo                     ( Todo
                                                , NewTodo
                                                )
import           Api                            ( TodoApi )


elmPath :: FilePath
elmPath = "../client/src/Requests/Todo.elm"

options :: ElmOptions
options = defElmOptions { urlPrefix = Static "http://localhost:3030" }

elmHeader :: Text -> Text
elmHeader elmModule =
  "module " <> elmModule <> " exposing (..)\n\n" <> defElmImports


elmfile :: [Text]
elmfile =
  elmHeader "Requests.Todo"
    : "type NoContent\n    = NoContent"
    : toElmTypeSource (Proxy :: Proxy Todo)
    : toElmTypeSource (Proxy :: Proxy NewTodo)
    : toElmDecoderSource (Proxy :: Proxy Todo)
    : toElmEncoderSource (Proxy :: Proxy Todo)
    : toElmEncoderSource (Proxy :: Proxy NewTodo)
    : generateElmForAPIWith options (Proxy :: Proxy TodoApi)



generate :: IO ()
generate = do
  putStrLn $ "Writing Elm queries functions to " <> elmPath
  writeFile elmPath $ intercalate "\n\n\n" elmfile
