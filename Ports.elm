port module Ports exposing (..)


port initSizeInfo : () -> Cmd msg


port size : (( Int, Int ) -> msg) -> Sub msg
