{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module CodeGens.Docs where

import           Protolude
import           Data.Version                   ( showVersion )
import           Servant.Docs
import           Api                            ( todoApi )
import           Config                         ( apiVersion )
import           Data.Text                      ( pack )


docPath :: FilePath
docPath = "README.md"

intro :: DocIntro
intro = DocIntro
  "Todo API documentation"
  [ "This is a simple REST API in Servant for the even simpler Todo App."
  , "Elm query functions and API documentations are generated (servant-elm, servant-docs)"
  , "API version: " ++ showVersion apiVersion
  ]


generate :: IO ()
generate = do
  putStrLn $ "Writing API documentation to " <> docPath
  writeFile docPath $ pack . markdown $ docsWithIntros [intro] todoApi
