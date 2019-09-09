module CodeGens.Elm where

import           Protolude
import           Servant.Elm                    ( Proxy(Proxy)
                                                , DefineElm(..)
                                                , defElmOptions
                                                , defElmImports
                                                , generateElmModuleWith
                                                , ElmOptions(..)
                                                , UrlPrefix(..)
                                                )
import           Model.Todo                     ( Todo
                                                , NewTodo
                                                )
import           Api                            ( TodoApi )


elmOptions :: ElmOptions
elmOptions =
  defElmOptions { urlPrefix = Static "http://localhost:3030"}


generate :: IO ()
generate = generateElmModuleWith
  elmOptions
  ["Requests", "Todo"]
  defElmImports
  "../client/src"
  [DefineElm (Proxy :: Proxy Todo), DefineElm (Proxy :: Proxy NewTodo)]
  (Proxy :: Proxy TodoApi)
