package ru.spbau.jvm.scala
package lecture03
package parser

import org.scalatest.FunSuite
import ru.spbau.jvm.scala.lecture03.parser.messages.AddWord

/**
  * @author Alefas
  */
class MessageParserTest extends FunSuite {

  test("Добавить слово") {
    assertResult(AddWord("word", "слово")) {
      MessageParser.parse("Добавить слово word с переводом слово")
    }
  }
}
