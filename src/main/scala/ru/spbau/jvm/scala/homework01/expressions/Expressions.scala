package ru.spbau.jvm.scala.homework01.expressions

trait Expression {
  def eval(): Double
}

class MyNumber(val value: Double) extends Expression {
  def eval(): Double = value
}

/**
  Operator precedences and their precedence range were taken from ${url: https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages}
 */
trait Operator extends Expression

object Associativity extends Enumeration {
  type Associativity = Value
  val Right, Left = Value
}

object Expressions {
  def constructNumber(x: Double): MyNumber = new MyNumber(x)

  def constructUnaryOperator(name: String, x: Expression): UnaryOperator =
    operators.unaryOperators.info(name).constructor.apply(Seq(x))

  def constructBinaryOperator(name: String, x: Expression, y: Expression): BinaryOperator =
    operators.binaryOperators.info(name).constructor.apply(Seq(x, y))

  def constructMathFunction(name: String, args: Seq[Expression]): MathFunction =
    mathFunctions.info(name).constructor.apply(args)

  object operators {
    val unaryOperators: UnaryOperators.type = UnaryOperators
    val binaryOperators: BinaryOperators.type = BinaryOperators

    val precedenceRange: Range = 1 to math.max(
      unaryOperators.info.values.map((opInfo) => opInfo.precedence).max,
      binaryOperators.info.values.map((opInfo) => opInfo.precedence).max
    )
  }

  val mathFunctions: MathFunctions.type = MathFunctions
}