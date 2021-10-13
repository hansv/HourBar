module HourBar exposing (Model, Msg, initModel, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List



-- MODEL


type alias Model =
    { label : String
    , dragging : Dragger
    , hours : List OpenClosed
    , state : OpenClosed
    }


type Dragger
    = No
    | Drag OpenClosed


type OpenClosed
    = Open
    | Closed


allClosed : List OpenClosed
allClosed =
    List.range 0 23
        |> List.map (\_ -> Closed)


initModel : Model
initModel =
    { label = ""
    , dragging = No
    , hours = allClosed
    , state = Open
    }



-- MSG


type Msg
    = NoOp
    | Start Int OpenClosed
    | Step Int OpenClosed
    | Stop



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Start idx oc ->
            let
                hours =
                    List.updateAt idx (\_ -> oc) model.hours
            in
            ( { model | hours = hours, dragging = Drag oc, state = oc }, Cmd.none )

        Step idx oc ->
            let
                hours =
                    case model.dragging of
                        No ->
                            model.hours

                        _ ->
                            List.updateAt idx (\_ -> oc) model.hours
            in
            ( { model
                | hours = hours
              }
            , Cmd.none
            )

        Stop ->
            ( { model | dragging = No }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ div [] [ text model.label ]
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(24, minmax(0, 1fr))"
            , style "width" "100%"
            ]
            (List.indexedMap (viewBlock model.state) model.hours)
        ]


viewBlock : OpenClosed -> Int -> OpenClosed -> Html Msg
viewBlock state idx oc =
    let
        color =
            case oc of
                Open ->
                    "green"

                Closed ->
                    "grey"

        co =
            case oc of
                Open ->
                    Closed

                Closed ->
                    Open
    in
    div
        ([ style "background" color
         , style "text-align" "center"
         , style "padding" "0.5rem"
         , onMouseDown (Start idx co)
         , onMouseEnter (Step idx state)
         , onMouseUp Stop
         ]
            ++ userSelectNone
        )
        [ text (String.fromInt idx) ]


userSelectNone : List (Attribute msg)
userSelectNone =
    List.map (\key -> style key "none")
        [ "-webkit-touch-callout"
        , "-webkit-user-select"
        , "-khtml-user-select"
        , "-moz-user-select"
        , "-ms-user-select"
        , "user-select"

        -- , "pointer-events"
        ]

