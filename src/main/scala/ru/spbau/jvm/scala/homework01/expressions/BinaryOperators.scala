package ru.spbau.jvm.scala.homework01.expressions

import ru.spbau.jvm.scala.homework01.expressions.Associativity.{Associativity, Left}
import ru.spbau.jvm.scala.homework01.util.ReflectionHelper

trait BinaryOperator extends Operator

private[expressions] class Plus(val left: Expression, val right: Expression) extends BinaryOperator {
  def eval(): Double = left.eval() + right.eval()
}

private[expressions] class Minus(val left: Expression, val right: Expression) extends BinaryOperator {
  def eval(): Double = left.eval() - right.eval()
}

private[expressions] class Times(val left: Expression, val right: Expression) extends BinaryOperator {
  def eval(): Double = left.eval() * right.eval()
}

private[expressions] class Divide(val left: Expression, val right: Expression) extends BinaryOperator {
  def eval(): Double = left.eval() / right.eval()
}

class BinaryOperatorInfo(
                          val constructor: (Seq[AnyRef]) => BinaryOperator,
                          val precedence: Int,
                          val associativity: Associativity
                        )

/**
  * We won't allow operator names without special symbols, because they can be treated as a function
  */
object BinaryOperators {
  private[this] def constructBOI(
      clazz: java.lang.Class[_ <: BinaryOperator],
      precedence: Int,
      associativity: Associativity): BinaryOperatorInfo = new BinaryOperatorInfo(
    ReflectionHelper.getPrimaryConstructor(clazz),
    precedence,
    associativity
  )

  val info = Map(
    "+" -> constructBOI(classOf[Plus], 4, Left),
    "-" -> constructBOI(classOf[Minus], 4, Left),
    "*" -> constructBOI(classOf[Times], 3, Left),
    "/" -> constructBOI(classOf[Divide], 3, Left)
  )
}