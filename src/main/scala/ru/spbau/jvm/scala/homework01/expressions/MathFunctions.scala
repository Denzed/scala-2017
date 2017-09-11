package ru.spbau.jvm.scala.homework01.expressions

import ru.spbau.jvm.scala.homework01.util.ReflectionHelper

trait MathFunction extends Expression

private[expressions] class Sinus(val x: Expression) extends MathFunction {
  def eval(): Double = math.sin(x.eval())
}

class MathFunctionInfo(
                        val constructor: (Seq[AnyRef]) => MathFunction,
                        val argumentCount: Int
                      )

/**
  * We won't allow function names with special symbols and digits,
  * because they can be treated as an operator or a (partly) number
  */
object MathFunctions {
  private[this] def constructMFI(
                                  clazz: java.lang.Class[_ <: MathFunction],
                                  argumentCount: Int): MathFunctionInfo = new MathFunctionInfo(
    ReflectionHelper.getPrimaryConstructor(clazz),
    argumentCount
  )

  val info = Map(
    "sin" -> constructMFI(classOf[Sinus], 1),
  )
}