module CodeGens.Swagger where

import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (?~)
                                                )
import           Protolude
import           Data.Swagger
import           Servant.Swagger                ( toSwagger )
import           Data.Yaml.Pretty               ( encodePretty
                                                , defConfig
                                                )
import           Api                            ( todoApi )

docPath :: FilePath
docPath = "swagger.yaml"

todoSwagger :: Swagger
todoSwagger =
  toSwagger todoApi
    & (info . title .~ "Todo API")
    & (info . version .~ "1.0")
    & (info . description ?~ "This is an API that tests swagger integration")
    & (info . license ?~ ("MIT" & url ?~ URL "http://mit.com"))


generate :: IO ()
generate = encodePretty defConfig todoSwagger & decodeUtf8 & writeFile docPath
