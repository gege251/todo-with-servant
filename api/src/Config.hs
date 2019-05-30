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


data DBConfig = DBConfig
    { dbHost :: Maybe [Char]
    , dbName :: Maybe [Char]
    , dbUser :: Maybe [Char]
    , dbPass :: Maybe [Char]
    }


toPGConfig :: DBConfig -> PGDatabase
toPGConfig (DBConfig host name user password) = defaultPGDatabase
  { pgDBHost = fallbackToDefault pgDBHost host
  , pgDBName = fallbackToDefault pgDBName (mapToByteString name)
  , pgDBUser = fallbackToDefault pgDBUser (mapToByteString user)
  , pgDBPass = fallbackToDefault pgDBPass (mapToByteString password)
  }
 where
  mapToByteString = map (strConv Strict)
  fallbackToDefault field = fromMaybe (field defaultPGDatabase)
