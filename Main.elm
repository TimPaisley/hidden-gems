module Main exposing (..)

import Geolocation as Geo
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (svg)
import Svg.Attributes exposing (fill, points, viewBox)
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { locations : List Location
    , gems : Int
    , error : String
    }


type alias Location =
    { coords : ( Float, Float )
    , name : String
    , img : String
    , cost : Int
    , found : Bool
    }


initialModel : Model
initialModel =
    let
        locations =
            [ { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "Solace of Wind"
              , img = "solace-of-wind.png"
              , cost = 5
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "The Beehive"
              , img = "beehive.png"
              , cost = 8
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "Wellington Cable Car"
              , img = "cable-car.png"
              , cost = 8
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "Kelburn Park"
              , img = "kelburn-park.png"
              , cost = 10
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "ASB Tower"
              , img = "asb-tower.png"
              , cost = 10
              , found = False
              }
            ]
    in
        { locations = locations, gems = 0, error = "" }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = LocationClicked Location
    | CheckLocation (Result Geo.Error Geo.Location)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationClicked location ->
            if location.found then
                ( model, Cmd.none )
            else
                ( model, Task.attempt CheckLocation Geo.now )

        CheckLocation result ->
            case result of
                Err error ->
                    Debug.log "Error Occurred: " error
                        |> \error -> ( { model | error = matchGeoError error }, Cmd.none )

                Ok geoloc ->
                    Debug.log "Location Received: " geoloc
                        |> \_ -> ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


matchGeoError : Geo.Error -> String
matchGeoError error =
    case error of
        Geo.PermissionDenied err ->
            "Permission Denied: " ++ err

        Geo.LocationUnavailable err ->
            "Location Unavailable: " ++ err

        Geo.Timeout err ->
            "Timeout: " ++ err



-- VIEW


gemIcon : Int -> String -> Html msg
gemIcon size color =
    span [ class "icon" ]
        [ svg [ width size, height size, viewBox "0 0 26 26" ]
            [ Svg.polygon [ fill color, points "6.5 3 19.5 3 26 9.66666667 13 23 0 9.66666667" ] [] ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ header model.gems
        , locations model.locations
        ]


header : Int -> Html Msg
header gems =
    div [ id "header" ]
        [ div [ class "container" ]
            [ div [ class "title" ] [ text "Hidden Gems" ]
            , div [ class "corner" ] [ gemIcon 20 "#FFF", text <| toString gems ]
            ]
        ]


locations : List Location -> Html Msg
locations locations =
    div [ id "locations" ]
        [ div [ class "container" ]
            (List.map locationCard locations)
        ]


locationCard : Location -> Html Msg
locationCard location =
    let
        title =
            if location.found then
                text location.name
            else
                text "Undiscovered"

        description =
            if location.found then
                text "You found this location! Tap here to learn more about it."
            else
                text "Find this location and tap here to claim your gems..."
    in
        div [ class "card", onClick (LocationClicked location) ]
            [ img [ class "image", src <| "images/" ++ location.img ] []
            , div [ class "content" ]
                [ div [ class "row" ]
                    [ div [ class "title" ] [ title ]
                    , div [ class "gems" ] [ gemIcon 13 "#000", text <| toString location.cost ]
                    ]
                , div [ class "description" ] [ description ]
                ]
            ]
