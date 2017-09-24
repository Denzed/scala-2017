package ru.spbau.jvm.scala.homework03.parser

import ru.spbau.jvm.scala.homework03.parser.messages._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  val wordParser: Parser[String] = raw"\S+".r
  val directionParser: Parser[String] =
    """(?i)\Qnorth\E""".r |
    """(?i)\Qeast\E""".r |
    """(?i)\Qsouth\E""".r |
    """(?i)\Qwest\E""".r
  val intParser: Parser[Int] = "[1-9][0-9]*".r ^^ {
    _.toInt
  }

  val start: Parser[UserMessage] =
    "[Ss]tart".r ~> intParser ~ ("x" ~> intParser) ^^ (dimensions => StartMessage(dimensions._1, dimensions._2))

  val go: Parser[UserMessage] =
    "[Gg]o".r ~> directionParser ~ intParser ^^ {tuple => GoMessage(tuple._1, tuple._2)}

  val position: Parser[UserMessage] =
  "[Pp]osition".r ^^ {_ => GetPositionMessage}

  val finish: Parser[UserMessage] =
    "[Ff]inish".r ^^ {_ => FinishMessage}

  val userMessage: Parser[UserMessage] =
    start | go | position | finish
}

object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}
