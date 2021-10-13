module Main exposing (main)

import Browser
import HourBar as HB
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import List.Extra as List



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = lazy view
        }



-- MODEL


type alias Model =
    { days : ( HB.Model, List HB.Model )
    , exceptions : List HB.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        hbm =
            HB.initModel

        monday =
            { hbm | label = "Monday" }
    in
    ( { days = ( monday, [ monday ] )
      , exceptions = []
      }
    , Cmd.none
    )



-- MSG


type Msg
    = NoOp
    | HBmsg HB.Model HB.Msg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HBmsg hbModel hbMsg ->
            let
                ( m, mCmd ) =
                    HB.update hbMsg hbModel

                days =
                    List.updateIf (\hbm -> hbm.label == m.label) (\_ -> m) (getDays model.days)
            in
            ( { model | days = ( m, days ) }, mCmd |> Cmd.map (HBmsg m) )


getDays : ( HB.Model, List HB.Model ) -> List HB.Model
getDays ml =
    let
        ( _, days ) =
            ml
    in
    days



-- View


view : Model -> Html Msg
view model =
    div [] (List.map dayView (getDays model.days))


dayView : HB.Model -> Html Msg
dayView hbm =
    HB.view hbm |> Html.map (HBmsg hbm)



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
