module CodeGens.Elm where

import           Protolude
import           Servant.Elm                    ( Proxy(Proxy)
                                                , DefineElm(..)
                                                , defElmOptions
                                                , defElmImports
                                                , generateElmModuleWith
                                                )
import           Model.Todo                     ( Todo
                                                , NewTodo
                                                )
import           Api                            ( TodoApi )


generate :: IO ()
generate = generateElmModuleWith
  defElmOptions
  ["Requests", "Todo"]
  defElmImports
  "../client/src"
  [DefineElm (Proxy :: Proxy Todo), DefineElm (Proxy :: Proxy NewTodo)]
  (Proxy :: Proxy TodoApi)
