module Board exposing (Board, Pos, Value, initialBoard, updateBoard)


type Value
    = X
    | O


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Board =
    List Cell


type alias Cell =
    { pos : Pos
    , val : Maybe Value
    }


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


updateBoard : Board -> Pos -> Value -> Board
updateBoard board pos v =
    List.map
        (\cell ->
            if cell.pos.x == pos.x && cell.pos.y == pos.y then
                { pos = pos, val = Just v }

            else
                cell
        )
        board
