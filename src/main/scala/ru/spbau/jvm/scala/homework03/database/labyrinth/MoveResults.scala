package ru.spbau.jvm.scala.homework03.database.labyrinth

trait MoveResult

case object MoveSuccessful extends MoveResult

case object MoveBlockedByWall extends MoveResult

case class MoveToExit(moves: Int) extends MoveResult

case object NoLabyrinth extends MoveResult

case object WrongDirection extends MoveResult
