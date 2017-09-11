package ru.spbau.jvm.scala.homework01.expressions

import ru.spbau.jvm.scala.homework01.util.ReflectionHelper

trait UnaryOperator extends Operator

private[expressions] class UnaryMinus(val x: Expression) extends UnaryOperator {
  override def eval(): Double = -x.eval()
}

class UnaryOperatorInfo(
                        val constructor: (Seq[AnyRef]) => UnaryOperator,
                        val precedence: Int
                       )

/**
  * We won't allow operator names without special symbols, because they can be treated as a function
  */
object UnaryOperators {
  private[this] def constructUOI(
                                  clazz: java.lang.Class[_ <: UnaryOperator],
                                  precedence: Int): UnaryOperatorInfo = new UnaryOperatorInfo(
    ReflectionHelper.getPrimaryConstructor(clazz),
    precedence
  )
  val info = Map(
    "-" -> constructUOI(classOf[UnaryMinus], 2),
  )
}