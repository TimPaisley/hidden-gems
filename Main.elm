module Main exposing (..)

import Geolocation as Geo
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as ListX
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
    , lastKnownLocation : Maybe Geo.Location
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
              , img = "solace-of-wind.jpg"
              , cost = 5
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "The Beehive"
              , img = "beehive.jpg"
              , cost = 8
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "Wellington Cable Car"
              , img = "cable-car.jpg"
              , cost = 8
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "Kelburn Park"
              , img = "kelburn-park.jpg"
              , cost = 10
              , found = False
              }
            , { coords = ( -41.28918850000001, 174.77715709999998 )
              , name = "ASB Tower"
              , img = "asb-tower.jpg"
              , cost = 10
              , found = False
              }
            ]
    in
        { locations = locations, gems = 0, error = "", lastKnownLocation = Nothing }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = LocationClicked Location
    | GetNewLocation
    | AddCurrentLocation
    | ResetAllLocations
    | NewLocation (Result Geo.Error Geo.Location)
    | CheckLocation Location (Result Geo.Error Geo.Location)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationClicked location ->
            if location.found then
                ( model, Cmd.none )
            else
                ( model, Task.attempt (CheckLocation location) Geo.now )

        GetNewLocation ->
            ( model, Task.attempt NewLocation Geo.now )

        AddCurrentLocation ->
            case model.lastKnownLocation of
                Just l ->
                    let
                        tempLocation =
                            { coords = ( l.latitude, l.longitude )
                            , name = "Placeholder"
                            , img = "solace-of-wind.png"
                            , cost = 0
                            , found = False
                            }
                    in
                        ( { model | locations = tempLocation :: model.locations }, Cmd.none )

                Nothing ->
                    ( { model | error = "Record a location before adding a placeholder." }, Cmd.none )

        ResetAllLocations ->
            ( { model | locations = List.map (\l -> { l | found = False }) model.locations }, Cmd.none )

        NewLocation result ->
            case result of
                Err error ->
                    ( { model | error = matchGeoError error }, Cmd.none )

                Ok geoloc ->
                    ( { model | lastKnownLocation = Just geoloc }, Cmd.none )

        CheckLocation location result ->
            case result of
                Err error ->
                    Debug.log "Error Occurred: " error
                        |> \error -> ( { model | error = matchGeoError error }, Cmd.none )

                Ok geoloc ->
                    let
                        ( newLocations, matchedLocation ) =
                            matchGeoResult geoloc location model.locations

                        gemsEarned =
                            if matchedLocation then
                                location.cost
                            else
                                0

                        debug =
                            Debug.log "Within Range" matchedLocation
                    in
                        Debug.log "Location Received" geoloc
                            |> \_ -> ( { model | locations = newLocations, gems = model.gems + gemsEarned, lastKnownLocation = Just geoloc }, Cmd.none )

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


matchGeoResult : Geo.Location -> Location -> List Location -> ( List Location, Bool )
matchGeoResult geoloc location locations =
    let
        proximity =
            0.0001

        isWithin ( x1, y1 ) ( x2, y2 ) =
            (abs (x1 - x2)) < proximity && (abs (y1 - y2)) < proximity

        locationIsWithinRange =
            isWithin location.coords ( geoloc.latitude, geoloc.longitude )
    in
        ( ListX.replaceIf (\l -> l.name == location.name) { location | found = locationIsWithinRange } locations, locationIsWithinRange )



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
        , debugPanel model
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


debugPanel : Model -> Html Msg
debugPanel model =
    let
        errors =
            if model.error == "" then
                "No errors"
            else
                model.error

        lastKnownLocationStr =
            case model.lastKnownLocation of
                Just l ->
                    toString l.latitude ++ ", " ++ toString l.longitude

                Nothing ->
                    "N/A"
    in
        div [ id "debug" ]
            [ div [ class "container" ]
                [ div [ class "card" ]
                    [ div [ class "content" ]
                        [ div [ class "row" ]
                            [ div [ class "title" ] [ text "Debug" ]
                            , div [] [ text <| toString (List.length model.locations) ++ " locations" ]
                            ]
                        , div [ class "description" ] [ text errors ]
                        , div [ class "row" ]
                            [ div [] [ text <| "Last Known Location: " ++ lastKnownLocationStr ]
                            ]
                        , div [ class "row" ]
                            [ button [ onClick GetNewLocation ] [ text "Get Location" ]
                            , button [ onClick AddCurrentLocation ] [ text "Add Current Location" ]
                            , button [ onClick ResetAllLocations ] [ text "Reset All Locations" ]
                            ]
                        ]
                    ]
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
