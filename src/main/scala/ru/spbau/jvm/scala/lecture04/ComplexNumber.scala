package ru.spbau.jvm.scala.lecture04

case class ComplexNumber(x: Double = 0, y: Double = 0) {

  def +(complexNumber: ComplexNumber): ComplexNumber = {
    val ComplexNumber(x1, y1) = complexNumber
    ComplexNumber(x + x1, y + y1)
  }
}
