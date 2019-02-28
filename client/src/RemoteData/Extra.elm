module RemoteData.Extra exposing (fromResult)

import Http
import RemoteData exposing (WebData)


fromResult : Result ( Maybe (Http.Metadata, String), Http.Error ) a -> WebData a
fromResult =
    Result.mapError Tuple.second >> RemoteData.fromResult
