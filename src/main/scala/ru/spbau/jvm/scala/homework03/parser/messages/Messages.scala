package ru.spbau.jvm.scala.homework03.parser.messages

import ru.spbau.jvm.scala.homework03.database.labyrinth.Direction

trait UserMessage

case class StartMessage(width: Int, height: Int) extends UserMessage

case object FinishMessage extends UserMessage

case class GoMessage(direction: Direction, steps: Int) extends UserMessage

case object GetPositionMessage extends UserMessage

case object WrongMessage extends UserMessage
