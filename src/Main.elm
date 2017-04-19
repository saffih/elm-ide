module Main exposing (..)

import Html
import State
import View


main =
    Html.program
        { init = State.init
        , update = State.update
        , view = View.root
        , subscriptions = State.subscriptions
        }
