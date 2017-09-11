package ru.spbau.jvm.scala.homework01

import ru.spbau.jvm.scala.homework01.expressions.Expression
import ru.spbau.jvm.scala.homework01.util.Parser

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("No expression supplied")
    } else {
      val result: Option[Expression] = Parser.parse(args(0))
      if (result.nonEmpty)
        println(result.get.eval())
      else
        println("Given string is not an expression")
    }
  }
}
