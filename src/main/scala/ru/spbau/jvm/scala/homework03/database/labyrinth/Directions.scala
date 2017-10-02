package ru.spbau.jvm.scala.homework03.database.labyrinth

trait Direction {
  val dx: Int
  val dy: Int
  val isDefined: Boolean = false
}

case object Undefined extends Direction {
  override val dx: Int = ???
  override val dy: Int = ???
}

case object North extends Direction {
  override val dx: Int = 0
  override val dy: Int = -1
  override val isDefined: Boolean = true
}

case object East extends Direction {
  override val dx: Int = 1
  override val dy: Int = 0
  override val isDefined: Boolean = true
}

case object South extends Direction {
  override val dx: Int = 0
  override val dy: Int = 1
  override val isDefined: Boolean = true
}

case object West extends Direction {
  override val dx: Int = -1
  override val dy: Int = 0
  override val isDefined: Boolean = true
}

object Direction {
  val directions = 4
  val directionsByName: Map[String, Direction] = Map(
    "north" -> North,
    "east" -> East,
    "south" -> South,
    "west" -> West
  )

  def stepInDirection(position: (Int, Int), dir: Direction): Option[(Int, Int)] =
    if (dir.isDefined) for {(x, y) <- Option(position)} yield (x + dir.dx, y + dir.dy) else Option.empty

  def randomDirection(): Direction =
    (scala.math.random() * 4).toInt match {
      case 0 => North
      case 1 => East
      case 2 => South
      case 3 => West
    }

  def nextDirectionClockwise(direction: Direction): Direction =
    direction match {
      case North => East
      case East => South
      case South => West
      case West => North
    }
}