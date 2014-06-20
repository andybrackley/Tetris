namespace TetrisModel

[<AutoOpen>]
module TetrisModule =

  type Colour = Blue | Green | Yellow | Pink | Red

  type ShapeName = 
    | L
    | Square
    | Line
    | S
    | Z

  type Rotate = Clockwise | AntiClockwise
  type Direction = Left | Right | Down
  type Rotation = North | East | South | West
  type Point = { X : int; Y : int }

  type Shape = { name : ShapeName; topLeft : Point; currentRotation : Rotation }

  type GameBoard = { ActiveShape : Shape ; Shapes : Shape list }
  type Game = { Score : int; Level : int;  }

  let shapeColour = function
    | L -> Blue
    | Square -> Green
    | Line -> Yellow
    | S -> Pink
    | Z -> Red

  let ShapeMoved shape = function
    | Left -> { shape with topLeft = { X = shape.topLeft.X - 1; Y = shape.topLeft.Y } }
    | Right -> { shape with topLeft = { X = shape.topLeft.X + 1; Y = shape.topLeft.Y } }
    | Down -> { shape with topLeft = { Y = shape.topLeft.Y - 1; X = shape.topLeft.Y } }

  let ShapeRotated shape = function
    | Clockwise -> 
        match shape.currentRotation with
        | North -> { shape with currentRotation = East }
        | East -> { shape with currentRotation = South }
        | South -> { shape with currentRotation = West }
        | West -> { shape with currentRotation = North }
    | AntiClockwise -> 
        match shape.currentRotation with
        | North -> { shape with currentRotation = West }
        | East -> { shape with currentRotation = North }
        | South -> { shape with currentRotation = East }
        | West -> { shape with currentRotation = South }

  let isPositionValid gameBoard shape =
    shape.topLeft.X >= 0

  let hitBottom gameBoard shape =
    shape.topLeft.Y = 0

  let withShapeAdded gameBoard shape =
    { gameBoard with Shapes = shape :: gameBoard.Shapes }

  let ActiveShapeMoved gameBoard direction =
    let shiftedShape = gameBoard.ActiveShape |> ShapeMoved <| direction
    if isPositionValid gameBoard shiftedShape then
      { gameBoard with ActiveShape = shiftedShape }
    else if hitBottom gameBoard shiftedShape then
      withShapeAdded gameBoard shiftedShape
    else
      gameBoard


