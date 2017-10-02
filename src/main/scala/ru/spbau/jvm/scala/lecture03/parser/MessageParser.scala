package ru.spbau.jvm.scala
package lecture03
package parser

import ru.spbau.jvm.scala.lecture03.parser.messages._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * @author Alefas
  */
class MessageParser extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  val wordParser: Parser[String] = raw"\S+".r
  val intParser: Parser[Int] = "[1-9][0-9]*".r ^^ {
    _.toInt
  }

  val addWord: Parser[AddWord] =
    "[Дд]обавить слово".r ~> wordParser ~ ("с переводом" ~> wordParser) ^^ {
      case word ~ translation => AddWord(word, translation)
    }

  val myWords: Parser[UserMessage] = "[Мм]ои слова[.]?".r ^^ { _ => MyWords }

  val testMe: Parser[UserMessage] = "[Пп]роверь меня!".r ^^ { _ => CheckMe }

  val userMessage: Parser[UserMessage] =
    addWord | myWords | testMe
}

object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}
