import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Element
import Collage
import Color exposing (Color)
import Keyboard
import List.Extra
import Char
import Maybe exposing (andThen, withDefault)

type alias Model =
  { grid : Grid
  , piece : Piece
  }

type alias Grid =
  { cells : Cells
  , width : Int
  , height : Int
  }

type alias Piece =
  { pieceType : PieceType
  , color : Color
  , blocks : List Pos
  , x : Int
  , y : Int
  }

type alias Pos =
  { x : Int
  , y : Int
  }

type alias Cells =
  List Row

type Row =
  List Block

type Msg =
  KeyMsg Keyboard.KeyCode

type Block =
  EmptyBlock | FilledBlock Color

type PieceType =
  I | S | Z | J | L | O

type Direction =
  Down | Left | Right

main =
  App.program { init = init
              , view = view
              , update = update
              , subscriptions = subscriptions
              }

makePiece pieceType =
  case pieceType of
    I -> (Color.lightBlue, [{x=0, y=-1},  {x=0, y=0},  {x=0, y=1}, {x=0, y=2}])
    Z -> (Color.red,       [{x=0, y=-1},  {x=-1, y=0}, {x=0, y=0}, {x=-1, y=1}])
    J -> (Color.blue,      [{x=0, y=-1},  {x=0, y=0},  {x=0, y=0}, {x=-1, y=1}])
    S -> (Color.green,     [{x=-1, y=-1}, {x=-1, y=0}, {x=0, y=0}, {x=0, y=1}])
    L -> (Color.orange,    [{x=0, y=-1},  {x=0, y=0},  {x=0, y=1}, {x=1, y=1}])
    O -> (Color.yellow,    [{x=0, y=0},   {x=0, y=1},  {x=1, y=0}, {x=1, y=1}])

stampPiece piece grid =
  List.foldl (stampBlock piece) grid piece.blocks

stampBlock piece block grid =
  let
    gridX = piece.x+block.x
    gridY = piece.y+block.y
    newCells = withDefault grid.cells
               ((List.Extra.getAt gridY grid.cells) `andThen`
                  (List.Extra.setAt gridX (FilledBlock piece.color)) `andThen`
                  (\newRow -> List.Extra.setAt gridY newRow grid.cells))
  in
    {grid | cells = newCells}

blockSize = 20

init =
  (initModel 10 20, Cmd.none)

initModel w h =
  { grid = initGrid w h
  , piece = initPiece 5 5 Z
  }

initPiece : Int -> Int -> PieceType -> Piece
initPiece x y pieceType =
  let
    (color, blocks) = makePiece pieceType
  in
    { color = color
    , blocks = blocks
    , pieceType = pieceType
    , x = x
    , y = y
    }

initGrid w h =
  { cells = List.repeat h (List.repeat w EmptyBlock)
  , width = w
  , height = h
  }

update msg model =
  -- TODO: add time messages to enable gravity
  case msg of
    KeyMsg code ->
     (handleKeyPress code model |> ensureLegal model, Cmd.none)

handleKeyPress code model =
  case Char.fromCode code of
    'a' -> {model | piece = move Left model.piece}
    'd' -> {model | piece = move Right model.piece}
    's' -> {model | piece = move Down model.piece}
    'w' -> {model | piece = rotate model.piece}
    _   -> model

rotate piece =
  -- TODO: fix better rotation
  {piece | blocks = List.map rotateBlock piece.blocks}

rotateBlock {x, y} =
  {x=-y, y=x}

move dir piece =
  case dir of
    Left  -> {piece | x = piece.x - 1}
    Right -> {piece | x = piece.x + 1}
    Down  -> {piece | y = piece.y + 1}

ensureLegal oldModel newModel =
  if List.all (isLegalPos newModel.piece newModel.grid) newModel.piece.blocks
  then
    newModel
  else
    oldModel

isLegalPos piece grid block =
  let
    x = piece.x + block.x
    y = piece.y + block.y
  in
    (0 <= x)
      && (x < grid.width)
      && (0 <= y)
      && (y < grid.height)


view model =
  Collage.collage
           (model.grid.width * blockSize * 2)
           (model.grid.height * blockSize * 2)
           [([(viewGrid (stampPiece model.piece model.grid))]
           |> Collage.group
           |> Collage.moveY (toFloat (model.grid.height * blockSize)))]
           |> Element.toHtml

viewGrid grid =
  List.map2 (viewRow grid.width) [0..grid.height] grid.cells
  |> Collage.group

viewRow width ypos row =
  List.map2 viewBlock [0..width] row
    |> Collage.group
    |> Collage.moveY (toFloat (-ypos * blockSize))

viewBlock xpos block =
  colorBlock block |> Collage.moveX (toFloat (xpos * blockSize))

colorBlock block =
  case block of
    EmptyBlock -> redSquare
    FilledBlock color -> Collage.filled color square

redSquare =
  Collage.outlined (Collage.solid Color.red) square

blueSquare =
  Collage.filled Color.blue square

square =
  Collage.rect (toFloat blockSize) (toFloat blockSize)

subscriptions _ =
  Sub.batch [ Keyboard.presses KeyMsg ]
