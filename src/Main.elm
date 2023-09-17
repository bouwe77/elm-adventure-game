module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Events exposing (onClick)



---- GAME definition ----


type alias Game =
    { startLocation : Location
    , locations : List Location
    }


field : Location
field =
    { id = "field"
    , text = "You are in a field. There is a castle to the north."
    , destinations =
        [ { name = "Go north", location = "castle" }
        ]
    }


castle : Location
castle =
    { id = "castle"
    , text = "You are in the castle. There is a field to the south."
    , destinations =
        [ { name = "Go south", location = "field" }
        ]
    }


game : Game
game =
    { startLocation = field
    , locations =
        [ field
        , castle
        ]
    }



---- MODEL ----


type alias Destination =
    { name : String
    , location : Identifier
    }


type alias Identifier =
    String


type alias Location =
    { id : Identifier
    , text : String
    , destinations : List Destination
    }


type alias Model =
    { currentLocation : Location
    , locations : List Location
    }


init : ( Model, Cmd Msg )
init =
    ( { currentLocation = game.startLocation
      , locations = game.locations
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = GoToLocation Identifier


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToLocation locationId ->
            ( { model | currentLocation = getLocation locationId model }
            , Cmd.none
            )


getLocation : Identifier -> Model -> Location
getLocation locationId model =
    List.filter (\location -> location.id == locationId) model.locations
        |> List.head
        |> Maybe.withDefault model.currentLocation



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Adventure Game" ]

        -- , case model.currentLocation of
        --     Nothing ->
        --         text "Nope"
        --     Just location ->
        --         viewCurrentLocation location
        , viewCurrentLocation model.currentLocation
        , viewDestinations model.currentLocation.destinations
        ]


viewCurrentLocation : Location -> Html Msg
viewCurrentLocation location =
    div []
        [ p [] [ text location.text ]
        ]


viewDestinations : List Destination -> Html Msg
viewDestinations destinations =
    div [] (List.map viewDestination destinations)


viewDestination : Destination -> Html Msg
viewDestination destination =
    div []
        [ button [ onClick (GoToLocation destination.location) ] [ text destination.name ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
