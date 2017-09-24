package ru.spbau.jvm.scala.homework03.parser.messages

trait UserMessage

case class Start(width: Int, height: Int) extends UserMessage

trait GoDirection extends UserMessage {
  val steps: Int
}

case object WrongMessage extends UserMessage
