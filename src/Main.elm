module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Events exposing (onClick)



--ja hiero
-- Voor items en actions:
-- Items moeten hun eigen actions hebben, bijvoorbeeld "drop"
-- En locaties moeten hun eigen actions hebben, bijvoorbeeld "open"
-- En of die locatie actie dan mogelijk is, hangt af van de items die je hebt
---- TYPES ----


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
    { id : String
    , dropName : String
    , getName : String
    , text : String
    }


type alias GameDefinition =
    { name : String
    , startLocation : Location
    , locations : List Location
    }



---- GAME definition ----


key : InventoryItem
key =
    { id = "key", getName = "Get key", dropName = "Drop key", text = "But wait, what's that, there is a key next to that oak tree!" }


field : Location
field =
    { id = "field"
    , text = "You are in a field. There is a castle to the north."
    , destinations =
        [ { name = "Go north", location = "castle" }
        ]
    , items =
        [ key ]
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


game : GameDefinition
game =
    { name = "The Legend of Glamourgloaming Castle"
    , startLocation = field
    , locations =
        [ field
        , castle
        ]
    }



---- MODEL ----


type alias Game =
    { locations : List Location
    , player : Player
    }


type alias Model =
    Game


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
    | AddToInventory InventoryItem
    | DropInventoryItem InventoryItem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToLocation locationId ->
            let
                player =
                    model.player

                newLocation =
                    getLocation locationId model
            in
            ( { model | player = { player | location = newLocation } }, Cmd.none )

        AddToInventory item ->
            let
                player =
                    model.player

                location =
                    player.location

                newInventory =
                    item :: player.inventory

                newItems =
                    List.filter (\i -> i.id /= item.id) player.location.items
            in
            ( { model | player = { player | inventory = newInventory, location = { location | items = newItems } } }, Cmd.none )

        DropInventoryItem item ->
            let
                player =
                    model.player

                location =
                    player.location

                newInventory =
                    List.filter (\i -> i.id /= item.id) player.inventory

                newItems =
                    item :: player.location.items
            in
            ( { model | player = { player | inventory = newInventory, location = { location | items = newItems } } }, Cmd.none )


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
        , viewLocationItems model.player.location.items
        , viewDestinations model.player.location.destinations
        , viewInventory model.player.inventory
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


viewLocationItems : List InventoryItem -> Html Msg
viewLocationItems items =
    div [] (List.map viewLocationItem items)


viewLocationItem : InventoryItem -> Html Msg
viewLocationItem item =
    div []
        [ button [ onClick (AddToInventory item) ] [ text item.getName ]
        ]


viewInventory : List InventoryItem -> Html Msg
viewInventory items =
    div []
        [ p [] [ text "Inventory" ]
        , div [] (List.map viewInventoryItem items)
        ]


viewInventoryItem : InventoryItem -> Html Msg
viewInventoryItem item =
    div []
        [ button [ onClick (DropInventoryItem item) ] [ text item.dropName ]
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
