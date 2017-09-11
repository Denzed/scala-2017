package ru.spbau.jvm.scala.lecture01

object Main {

  def main(args: Array[String]): Unit = {
    val bar = new Bar(name = "name")
    bar.foo()
    new Baz().foo()
  }
}
