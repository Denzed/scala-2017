package ru.spbau.jvm.scala.homework03.database

trait MoveResult

case object MoveSuccessful extends MoveResult

case object MoveBlockedByWall extends MoveResult

case class MoveToExit(moves: Int) extends MoveResult

case object NoLabyrinth extends MoveResult

class Labyrinth(val width: Int, val height: Int) {
  val field: Array[Array[Int]] = generateField()
  var currentPosition: (Int, Int) = (1, 1)
  var moves: Int = 0

  val directionMap: Map[String,Int] = Map(
    "east" -> 0,
    "south" -> 1,
    "west" -> 2,
    "north" -> 3
  )

  def goTo(directionString: String, steps: Int): MoveResult  = {
    moves += 1

    var tempPosition = currentPosition

    val direction: Int = directionMap(directionString)

    for (_ <- 1 to steps) {
      tempPosition = updatePosition(direction, tempPosition._1, tempPosition._2)
      field(tempPosition._1)(tempPosition._2) match {
        case 1 => return MoveBlockedByWall
        case 2 => return MoveToExit(moves)
      }
    }
    currentPosition = tempPosition
    MoveSuccessful
  }

  private[this] def updatePosition(direction: Int, x: Int, y: Int): (Int, Int) =
    direction match {
      case 0 => (x + 1, y + 0)
      case 1 => (x + 0, y + 1)
      case 2 => (x - 1, y + 0)
      case _ => (x + 0, y - 1)
    }

  private[this] def carve(field: Array[Array[Int]], x: Int, y: Int) {
    var dir: Int = (scala.math.random * 4.0).toInt
    for (_ <- 1 to 4) {
      val (x1, y1) = updatePosition(dir, x, y)
      val (x2, y2) = updatePosition(dir, x1, y1)
      if(0 < x2 && x2 < width &&
         0 < y2 && y2 < height) {
        if(field(y1)(x1) == 1 &&
           field(y2)(x2) == 1) {
          field(y1)(x1) = 0
          field(y2)(x2) = 0
          carve(field, x2, y2)
        }
      }
      dir = (dir + 1) % 4
    }
  }

  def generateField(): Array[Array[Int]] = {
    val field = Array.fill[Int](height, width)(1)
    
    field(1)(1) = 0
    carve(field, 1, 1)
    field(0)(1) = 0
    field(height - 1)(width - 2) = 2

    field
  }
}