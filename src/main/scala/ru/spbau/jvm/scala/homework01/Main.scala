package ru.spbau.jvm.scala.homework01

import ru.spbau.jvm.scala.homework01.util.Parser

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("No expression supplied")
    } else {
      println(Parser.parse(args(0))
        .map(
          _.eval()
            .toString)
        .getOrElse("Given string is not an expression"))
    }
  }
}
