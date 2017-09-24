package ru.spbau.jvm.scala.homework03.parser.messages

trait UserMessage

case class StartMessage(width: Int, height: Int) extends UserMessage

case object FinishMessage extends UserMessage

case class GoMessage(direction: String, steps: Int) extends UserMessage

case object GetPositionMessage extends UserMessage

case object WrongMessage extends UserMessage
