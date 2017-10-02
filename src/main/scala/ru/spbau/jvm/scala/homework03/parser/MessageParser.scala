package ru.spbau.jvm.scala.homework03.parser

import ru.spbau.jvm.scala.homework03.database.labyrinth.{Direction, Undefined}
import ru.spbau.jvm.scala.homework03.parser.messages._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  val wordParser: Parser[String] = raw"\S+".r

  def directionParser(mapping: (String, Direction)): Parser[Direction] =
    s"""(?i)\\Q${mapping._1}\\E""".r.map(_ => mapping._2)

  val directionsParser: Parser[Direction] =
    Direction
      .directionsByName
      .map(directionParser)
      .reduce((a, b) => a | b) | ".*".r.map(_ => Undefined)

  val intParser: Parser[Int] = "[1-9][0-9]*".r ^^ {
    _.toInt
  }

  val start: Parser[UserMessage] =
    "[Ss]tart".r ~> intParser ~ ("x" ~> intParser) ^^ (dimensions => StartMessage(dimensions._1, dimensions._2))

  val go: Parser[UserMessage] =
    "[Gg]o".r ~> directionsParser ~ intParser ^^ {tuple => GoMessage(tuple._1, tuple._2)}

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
