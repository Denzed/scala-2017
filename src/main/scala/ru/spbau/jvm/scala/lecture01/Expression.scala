package ru.spbau.jvm.scala.lecture01

sealed trait Expression[T] {
  def eval(): T
}

case class