port module Notification exposing (sub)

import Bitwise exposing (and, shiftRightBy)
import Color exposing (Color, rgb)
import Json.Decode as JD exposing (field)
import Types exposing (..)

port notifications : (String -> msg) -> Sub msg

intToColor : Int -> Color
intToColor int =
    rgb (and (shiftRightBy int 16) 255) (and (shiftRightBy int 8) 255) (and int 255)

sub : Sub Msg
sub =
    notifications decode

decode : String -> Msg
decode notification =
    let
        result =
            JD.decodeString
                (JD.oneOf
                    [ systemDisconnect
                    , systemError
                    , systemReady
                    , redrawUpdateFg
                    , redrawUpdateBg
                    , redrawUpdateSp
                    , redrawResize
                    , redrawClear
                    , redrawEolClear
                    , redrawCursorGoto
                    , redrawPut
                    , redrawHighlightSet
                    , redrawScroll
                    , redrawSetScrollRegion
                    , redrawModeChange
                    , redrawBusyStart
                    , redrawBusyStop
                    , redrawMouseOn
                    , redrawMouseOff
                    , redrawBell
                    , redrawVisualBell
                    , redrawSetTitle
                    , redrawSetIcon
                    , JD.fail ("Unhandled notification: " ++ notification)
                    ]
                )
                notification
    in
        case result of
            Ok message ->
                message

            Err error ->
                Error error


first : JD.Decoder a -> JD.Decoder a
first decoder =
    JD.map identity decoder


systemError : JD.Decoder Msg
systemError =
    JD.at [ "system", "error" ] <|
        JD.map Error JD.string


systemDisconnect : JD.Decoder Msg
systemDisconnect =
    JD.at [ "system", "disconnect" ] <|
        JD.succeed Disconnect


systemReady : JD.Decoder Msg
systemReady =
    JD.at [ "system", "ready" ] <|
        JD.succeed Ready


redrawUpdateBg : JD.Decoder Msg
redrawUpdateBg =
    JD.at [ "redraw", "update_bg" ] <|
        first (JD.map (UpdateBg << intToColor) JD.int)


redrawUpdateFg : JD.Decoder Msg
redrawUpdateFg =
    JD.at [ "redraw", "update_fg" ] <|
        first (JD.map (UpdateFg << intToColor) JD.int)


redrawUpdateSp : JD.Decoder Msg
redrawUpdateSp =
    JD.at [ "redraw", "update_sp" ] <|
        first (JD.map (UpdateSp << intToColor) JD.int)


redrawResize : JD.Decoder Msg
redrawResize =
    JD.at [ "redraw", "resize" ] <|
        first (JD.map2 Resize JD.int JD.int)


redrawClear : JD.Decoder Msg
redrawClear =
    JD.at [ "redraw", "clear" ] <|
        first (JD.succeed Clear)


redrawEolClear : JD.Decoder Msg
redrawEolClear =
    JD.at [ "redraw", "eol_clear" ] <|
        JD.map (always EolClear) (JD.list (JD.succeed []))


redrawCursorGoto : JD.Decoder Msg
redrawCursorGoto =
    JD.at [ "redraw", "cursor_goto" ] <|
        first (JD.map2 CursorGoto JD.int JD.int)


redrawPut : JD.Decoder Msg
redrawPut =
    JD.at [ "redraw", "put" ] <|
        JD.map Put (JD.list (first JD.string))


redrawHighlightSet : JD.Decoder Msg
redrawHighlightSet =
    let
        parseColors fg bg sp =
            Highlight
                (Maybe.map intToColor fg)
                (Maybe.map intToColor bg)
                (Maybe.map intToColor sp)

        highlight =
            JD.map8 parseColors
                (JD.maybe (field "foreground" JD.int))
                (JD.maybe (field "background" JD.int))
                (JD.maybe (field "special"  JD.int))
                (JD.maybe (field "bold" JD.bool))
                (JD.maybe (field "italic" JD.bool))
                (JD.maybe (field "underline" JD.bool))
                (JD.maybe (field "undercurl" JD.bool))
                (JD.maybe (field "reverse" JD.bool))
    in
        JD.at [ "redraw", "highlight_set" ] <|
            JD.map HighlightSet (JD.list (first highlight))


redrawScroll : JD.Decoder Msg
redrawScroll =
    JD.at [ "redraw", "scroll" ] <|
        first (JD.map Scroll JD.int)


redrawSetScrollRegion : JD.Decoder Msg
redrawSetScrollRegion =
    JD.at [ "redraw", "set_scroll_region" ] <|
        first (JD.map4 SetScrollRegion JD.int JD.int JD.int JD.int)


redrawModeChange : JD.Decoder Msg
redrawModeChange =
    JD.at [ "redraw", "mode_change" ] <|
        (first (first (JD.map ModeChange JD.string)))


redrawBusyStart : JD.Decoder Msg
redrawBusyStart =
    JD.at [ "redraw", "busy_start" ] <|
        first (JD.succeed (Busy True))


redrawBusyStop : JD.Decoder Msg
redrawBusyStop =
    JD.at [ "redraw", "busy_stop" ] <|
        first (JD.succeed (Busy False))


redrawMouseOn : JD.Decoder Msg
redrawMouseOn =
    JD.at [ "redraw", "mouse_on" ] <|
        JD.map (always (Mouse True)) (JD.list (JD.succeed []))


redrawMouseOff : JD.Decoder Msg
redrawMouseOff =
    JD.at [ "redraw", "mouse_off" ] <|
        JD.map (always (Mouse False)) (JD.list (JD.succeed []))


redrawBell : JD.Decoder Msg
redrawBell =
    JD.at [ "redraw", "bell" ] <|
        JD.map (\bells -> Bell (List.length bells)) (JD.list (JD.succeed 0))


redrawVisualBell : JD.Decoder Msg
redrawVisualBell =
    JD.at [ "redraw", "visual_bell" ] <|
        JD.map (\bells -> VisualBell (List.length bells)) (JD.list (JD.succeed 0))


redrawSetTitle : JD.Decoder Msg
redrawSetTitle =
    JD.at [ "redraw", "set_title" ] <|
        first (JD.map SetTitle JD.string)


redrawSetIcon : JD.Decoder Msg
redrawSetIcon =
    JD.at [ "redraw", "set_icon" ] <|
        first (JD.map SetIcon JD.string)
