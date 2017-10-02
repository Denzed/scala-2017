package ru.spbau.jvm.scala
package lecture03
package parser
package messages

/**
  * @author Alefas
  */
trait UserMessage

case class AddWord(word: String, translation: String) extends UserMessage

case object MyWords extends UserMessage

case object CheckMe extends UserMessage

case object WrongMessage extends UserMessage
