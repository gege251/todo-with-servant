module Config where

import           Protolude
import           Database.PostgreSQL.Typed      ( PGDatabase(..)
                                                , defaultPGDatabase
                                                )
import           Data.Version                   ( Version
                                                , makeVersion
                                                )
import           Network.Wai.Middleware.Cors    ( Origin )


feHost :: Origin
feHost = "http://localhost:8000"


apiVersion :: Version
apiVersion = makeVersion [1, 0, 0]


filePath :: FilePath
filePath = "todos.json"


defaultPort :: Int
defaultPort = 3030


corsMethods :: [ByteString]
corsMethods = ["GET", "PUT", "DELETE", "OPTIONS"]


pgConfig :: PGDatabase
pgConfig =
  defaultPGDatabase { pgDBName = "gergo", pgDBUser = "postgres", pgDBPass = "" }
