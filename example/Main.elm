module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame)
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import SimpleTimer as SimpleTimer exposing (SimpleTimer)
import Task
import Time exposing (Posix)



-- TYPES


type Msg
    = Frame Posix
    | StartClicked
    | StartTimer Posix



-- UTILS


leftOf : b -> a -> ( a, b )
leftOf right left =
    ( left, right )


rightOf : a -> b -> ( a, b )
rightOf left right =
    ( left, right )



-- MODEL


type alias Model =
    SimpleTimer



-- INIT


init : ( Model, Cmd Msg )
init =
    ( SimpleTimer.create <| 3000, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartClicked ->
            ( model, Task.perform StartTimer Time.now )

        StartTimer now ->
            ( SimpleTimer.start now model, Cmd.none )

        Frame now ->
            ( SimpleTimer.tick now model, Cmd.none )



-- VIEW


viewTimer : Int -> Model -> Html Msg
viewTimer width model =
    case model.status of
        SimpleTimer.Started _ ->
            SimpleTimer.toSvg width model

        _ ->
            Html.div
                [ Attr.style "width" "500px"
                , Attr.style "height" "500px"
                ]
                [ Html.text "Timer not started!" ]


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.div []
            [ viewTimer 500 model ]
        , Html.button
            [ Events.onClick StartClicked ]
            [ Html.text "Start" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame (\posix -> Frame posix)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
