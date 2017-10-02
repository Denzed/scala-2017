package ru.spbau.jvm.scala.homework03.database.labyrinth

class Labyrinth private[Labyrinth](val width: Int, val height: Int) {
  val field: Array[Array[Cell]] = generateField()
  var currentPosition: (Int, Int) = (1, 1)
  var moves: Int = 0

  def goTo(direction: Direction, steps: Int): MoveResult  = {
    moves += 1

    var tempPosition = currentPosition

    for (_ <- 1 to steps) {
      Direction.stepInDirection(tempPosition, direction) match {
        case Some(newPosition) => tempPosition = newPosition
        case _                 => return WrongDirection
      }
      field(tempPosition._1)(tempPosition._2) match {
        case WallCell => return MoveBlockedByWall
        case ExitCell => return MoveToExit(moves)
        case EmptyCell =>
      }
    }
    currentPosition = tempPosition
    MoveSuccessful
  }

  private[this] def carve(field: Array[Array[Cell]], position: (Int,Int)) {
    val directions: List[Direction] =
      List.iterate(
        Direction.randomDirection(),
        Direction.directions)(
        Direction.nextDirectionClockwise)
    for (direction <- directions;
        position1@(x1, y1) <-
          Direction.stepInDirection(position, direction);
        position2@(x2, y2) <-
          Direction.stepInDirection(position1, direction)) {
      if(0 < x2 && x2 < width &&
          0 < y2 && y2 < height &&
          field(y1)(x1) == WallCell &&
          field(y2)(x2) == WallCell) {
        field(y1)(x1) = EmptyCell
        field(y2)(x2) = EmptyCell
        carve(field, position2)
      }
    }
  }

  def generateField(): Array[Array[Cell]] = {
    val field = Array.fill[Cell](height, width)(WallCell)
    field(1)(1) = EmptyCell
    carve(field, (1, 1))
    field(0)(1) = WallCell
    field(height - 1)(width - 2) = ExitCell

    field
  }
}

object Labyrinth {
  val minimumWidth: Int = 3
  val minimumHeight: Int = 3

  def apply(width: Int, height: Int): Option[Labyrinth] = {
    if (width < minimumWidth || height < minimumHeight)
      Option.empty
    else
      Some(new Labyrinth(width, height))
  }
}