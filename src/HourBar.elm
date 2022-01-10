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
    , interactive : Bool
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


initModel : String -> List OpenClosed -> Bool -> Model
initModel label hours interactive =
    let
        h =
            if List.isEmpty hours then
                allClosed

            else
                hours
    in
    { label = label
    , dragging = No
    , hours = h
    , state = Open
    , interactive = interactive
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
    let
        clst =
            model.hours
                |> List.indexedMap (\i oc -> ( List.getAt (i - 1) model.hours, oc, List.getAt (i + 1) model.hours ))
    in
    div [ style "display" "flex" ]
        [ div [] [ text model.label ]
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(24, minmax(0, 1fr))"
            , style "width" "100%"
            , class "text-white"
            ]
            (List.indexedMap (viewBlock model.interactive model.state) clst)
        ]


viewBlock : Bool -> OpenClosed -> Int -> ( Maybe OpenClosed, OpenClosed, Maybe OpenClosed ) -> Html Msg
viewBlock interactive state idx ocThrupple =
    let
        ( _, oc, _ ) =
            ocThrupple

        color =
            case oc of
                Open ->
                    "bg-green-800"

                Closed ->
                    "text-transparent hover:text-black"

        co =
            case oc of
                Open ->
                    Closed

                Closed ->
                    Open

        klss =
            let
                s =
                    "rounded-l-full"

                e =
                    "rounded-r-full"

                se =
                    "rounded-full"

                i =
                    "text-transparent hover:text-black"
            in
            case ocThrupple of
                ( Nothing, Open, Just Closed ) ->
                    se

                ( Just Closed, Open, Nothing ) ->
                    se

                ( Just Closed, Open, Just Closed ) ->
                    se

                --
                ( Just Open, Open, Just Closed ) ->
                    e

                ( Just Open, Open, Nothing ) ->
                    e

                --
                ( Just Closed, Open, Just Open ) ->
                    s

                --
                ( Just Open, Open, Just Open ) ->
                    i

                --
                _ ->
                    ""

        events =
            if interactive then
                [ onMouseDown (Start idx co)
                , onMouseEnter (Step idx state)
                , onMouseUp Stop
                ]

            else
                []
    in
    div
        ([ class color
         , style "text-align" "center"
         , style "padding" "0.5rem"
         , class klss
         ]
            ++ userSelectNone
            ++ events
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
