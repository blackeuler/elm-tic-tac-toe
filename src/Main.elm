module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Pos =
    { x : Int, y : Int }


type alias Model =
    { board : Board, turn : Value }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init =
    { board = initialBoard, turn = X }


view model =
    div
        [ style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-contents" "center"
        , style "align-items" "center"
        , style "padding" "20px"
        ]
        (if winnerp model.board then
            [ h1 [] [ text ((showValue <| switchTurn model.turn) ++ " is the Winner") ]
            , viewBoard model.board
            , button [ onClick ResetGame ] [ text "Play Again" ]
            ]

         else
            [ h1 [] [ text "Tic Tac Toe" ]
            , viewBoard model.board
            , button [ onClick ResetGame ] [ text "Reset " ]
            ]
        )


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellClicked pos ->
            { model | board = updateBoard model.board pos model.turn, turn = switchTurn model.turn }

        ResetGame ->
            init

        CantEdit ->
            model


updateBoard : Board -> Pos -> Value -> Board
updateBoard board pos v =
    List.map
        (\cell ->
            if cell.pos.x == pos.x && cell.pos.y == pos.y then
                Debug.log "Changed Board" { pos = pos, val = Just v }

            else
                cell
        )
        board


type alias Cell =
    { pos : Pos
    , val : Maybe Value
    }


type Value
    = X
    | O


switchTurn : Value -> Value
switchTurn v =
    case v of
        X ->
            O

        O ->
            X


type alias Board =
    List Cell


type Msg
    = CellClicked Pos
    | ResetGame
    | CantEdit


initialBoard : Board
initialBoard =
    [ { pos = { x = 1, y = 1 }, val = Nothing }
    , { pos = { x = 2, y = 1 }, val = Nothing }
    , { pos = { x = 3, y = 1 }, val = Nothing }
    , { pos = { x = 1, y = 2 }, val = Nothing }
    , { pos = { x = 2, y = 2 }, val = Nothing }
    , { pos = { x = 3, y = 2 }, val = Nothing }
    , { pos = { x = 1, y = 3 }, val = Nothing }
    , { pos = { x = 2, y = 3 }, val = Nothing }
    , { pos = { x = 3, y = 3 }, val = Nothing }
    ]


winnerp : Board -> Bool
winnerp b =
    majorDiag b || horizontal b || vertical b || minorDiagWin b


majorDiag : Board -> Bool
majorDiag b =
    let
        major =
            List.filter (\cell -> cell.pos.x == cell.pos.y) b

        mjV =
            List.map (\cell -> cell.val) major

        head =
            case List.head mjV of
                Nothing ->
                    Nothing

                Just v ->
                    v
    in
    if head == Nothing then
        False

    else
        List.all (\val -> val == head) mjV


minorDiagWin : Board -> Bool
minorDiagWin b =
    minorDiag b
        |> getValues
        |> sameButNothing


minorDiag : Board -> Board
minorDiag b =
    List.filter (\cell -> cell.pos == { x = 3, y = 1 } || cell.pos == { x = 2, y = 2 } || cell.pos == { x = 1, y = 3 }) b


horizontal : Board -> Bool
horizontal b =
    List.map (\r -> row r b) [ 1, 2, 3 ]
        |> List.map (\r -> getValues r)
        |> List.any sameButNothing


vertical : Board -> Bool
vertical b =
    List.map (\r -> col r b) [ 1, 2, 3 ]
        |> List.map (\r -> getValues r)
        |> List.any sameButNothing


row : Int -> Board -> Board
row r b =
    List.filter (\cell -> cell.pos.y == r) b


col : Int -> Board -> Board
col r b =
    List.filter (\cell -> cell.pos.x == r) b


sameButNothing : List (Maybe a) -> Bool
sameButNothing lst =
    let
        head =
            case List.head lst of
                Nothing ->
                    Nothing

                Just v ->
                    v
    in
    if head == Nothing then
        False

    else
        List.all (\val -> val == head) lst


getValues : List Cell -> List (Maybe Value)
getValues c =
    List.map (\cell -> cell.val) c


viewBoard : Board -> Html Msg
viewBoard board =
    createGrid 3 3 <| List.map (placeCell board) board


createGrid : Int -> Int -> List (Html.Html Msg) -> Html.Html Msg
createGrid rows cols =
    div
        [ style "display" "grid"
        , style "grid-template-rows" <|
            "repeat("
                ++ String.fromInt rows
                ++ ",auto)"
        , style "grid-template-columns" <|
            "repeat("
                ++ String.fromInt cols
                ++ ",auto)"
        , style "height" "50vh"
        , style "width" "50vh"
        ]


editable : Cell -> Board -> Bool
editable c board =
    if winnerp board then
        False

    else
        case c.val of
            Nothing ->
                True

            Just _ ->
                False


placeCell : Board -> Cell -> Html Msg
placeCell board cell =
    div
        [ style "grid-column" <| String.fromInt cell.pos.x
        , style "grid-row" <| String.fromInt cell.pos.y
        , style "border" "solid black 1px"
        , style "font-size" "60px"
        , style "text-align" "center"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , if editable cell board then
            onClick (CellClicked cell.pos)

          else
            onClick CantEdit
        ]
        [ case cell.val of
            Nothing ->
                div [ style "width" "30px", style "height" "30px" ]
                    []

            Just v ->
                text (showValue v)
        ]


showValue : Value -> String
showValue v =
    case v of
        X ->
            "X"

        O ->
            "O"


placeContent : List (Html.Html Msg) -> Pos -> Html Msg
placeContent stuff pos =
    div
        [ style "grid-column" <| String.fromInt pos.x
        , style "grid-row" <| String.fromInt pos.y
        , onClick (CellClicked pos)
        ]
        stuff



-- Make Board
-- A board consists of 9 cells
-- can use a table or div with grid
-- dont know which one is better will use grid to save time
-- need to interact with grid
-- should be able to place content on a grid of my choosing.
