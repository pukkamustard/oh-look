module Json.Decode.Applicative exposing (pure, apply)

import Json.Decode exposing (Decoder, succeed, map2)


pure : a -> Decoder a
pure =
    succeed


apply : Decoder a -> Decoder (a -> b) -> Decoder b
apply =
    map2 (|>)
