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
    , items = []
    }


castle : Location
castle =
    { id = "castle"
    , text = "You are in the castle. There is a field to the south."
    , destinations =
        [ { name = "Go south", location = "field" }
        ]
    , items = []
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


type alias Player =
    { name : String
    , location : Location
    , inventory : List InventoryItem
    }


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
    , items : List InventoryItem
    }


type alias InventoryItem =
    { name : String
    }


type alias Model =
    { locations : List Location
    , player : Player
    }


init : ( Model, Cmd Msg )
init =
    ( { locations = game.locations
      , player = { name = "Player 1", location = game.startLocation, inventory = [] }
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
            let
                newLocation = getLocation locationId model
            in
            ( { model | player = { model.player | location = newLocation.id } }, Cmd.none )
            


getLocation : Identifier -> Model -> Location
getLocation locationId model =
    List.filter (\location -> location.id == locationId) model.locations
        |> List.head
        |> Maybe.withDefault model.player.location



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Adventure Game" ]
        , viewCurrentLocation model.player.location
        , viewDestinations model.player.location.destinations
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
