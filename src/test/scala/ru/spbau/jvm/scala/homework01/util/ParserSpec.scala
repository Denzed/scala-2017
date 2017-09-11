package ru.spbau.jvm.scala.homework01.util

import org.scalatest._
import org.scalatest.matchers.Matchers

class ParserSpec extends FlatSpec with Matchers {
  def num(x: Double)

  "Parser" should "parse string to Double" in {
    Parser.parse("1").get.eval() must equal (1)
    Parser.parse("1.").get.eval() must equal (1)
    Parser.parse(".179").get.eval() must equal (0.179)
    Parser.parse("1.79").get.eval() must equal (1.79)
    Parser.parse("1.79.") must be ('empty)
    Parser.parse(".") must be ('empty)
  }

  "Parser" should "correctly work with unary operators" in {
    Parser.parse("-1").get.eval() must equal (-1)
    Parser.parse("--1").get.eval() must equal (1)
    Parser.parse("---1").get.eval() must equal (-1)
    Parser.parse("-1").get.eval() must equal (-1)
  }

  "Parser" should "correctly work with binary operators" in {
    Parser.parse("1+5").get.eval() must equal (6)
    Parser.parse("1-5").get.eval() must equal (-4)
    Parser.parse("1*5").get.eval() must equal (5)
    Parser.parse("1/5").get.eval() must equal (0.2)
  }

  "Parser" should "correctly parse operation precedence" in {
    Parser.parse("1+5/5").get.eval() must equal (2)
    Parser.parse("1-5/5").get.eval() must equal (0)
    Parser.parse("1/5+1").get.eval() must equal (6)
    Parser.parse("1/5-1").get.eval() must equal (-0.8)
    Parser.parse("1+5*5").get.eval() must equal (26)
    Parser.parse("1-5*5").get.eval() must equal (-24)
    Parser.parse("1*5+1").get.eval() must equal (6)
    Parser.parse("1*5-1").get.eval() must equal (4)
    Parser.parse("1+7+9").get.eval() must equal (17)
    Parser.parse("1-7-9").get.eval() must equal (-15)
    Parser.parse("1*7*9").get.eval() must equal (63)
    Parser.parse("63/1/7/9").get.eval() must equal (1)
  }

  "Parser" should "correctly parse functions" in {
    Parser.parse("sin(3.141)").get.eval() must be (0 plusOrMinus 0.001)
    Parser.parse("sin(3.141") must be ('empty)
    Parser.parse("sin()") must be ('empty)
    Parser.parse("1+sin(3.141)*1-1") must be (0 plusOrMinus 0.001)
  }
}
