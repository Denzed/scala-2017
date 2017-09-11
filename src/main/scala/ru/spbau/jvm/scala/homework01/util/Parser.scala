package ru.spbau.jvm.scala.homework01.util

import ru.spbau.jvm.scala.homework01.expressions._

/**
  * Class used for parsing a given string to expression.
  * For the sake of simplicity we assume that the expression contains no delimiters
  */
object Parser {
  /**
    * Parses the string from constructor to expression and checks that the whole
    * string has been parsed
    * @param string string to parse
    * @return parsed expression
    */
  def parse(string: String): Option[Expression] = {
    val result = parseExpression(string)
    if (result.nonEmpty && result.get._2.isEmpty)
      Option.apply(result.get._1)
    else
      Option.empty
  }

  private[this] def parseFunctionArguments(left: Int, string: String): Option[(Seq[Expression], String)] = {
    if (left == 0) {
      return Option.apply((Seq(), string))
    }
    val currentArgument = parseExpression(string)
    if (currentArgument.nonEmpty && currentArgument.get._2.startsWith(if (left > 1) "," else ")"))
      parseFunctionArguments(left - 1, currentArgument.get._2.substring(1))
          .map((nextArguments) => (
            nextArguments._1.+:(currentArgument.get._1),
            nextArguments._2
          ))
    else
      Option.empty
  }

  private[this] def isValidFunctionBeginning(string: String, name: String): Boolean =
    string.startsWith(name ++ "(")

  private[this] def parseFunction(string: String): Option[(Expression, String)] = {
    for ((name: String, info: MathFunctionInfo) <- Expressions
        .mathFunctions
        .info
        .filter((tuple) => isValidFunctionBeginning(string, tuple._1))) {
      val args =
        parseFunctionArguments(
          info.argumentCount,
          string.substring(name.length + 1)
        )
      if (args.nonEmpty) {
        return Option.apply((info.constructor(args.get._1), args.get._2))
      }
    }
    Option.empty
  }

  private[this] def parseUnaryOperator(string: String, currentPrecedence: Int): Option[(Expression, String)] = {
    for ((name, _) <- Expressions
        .operators
        .unaryOperators
        .info
        .filter((tuple) => tuple._2.precedence == currentPrecedence && string.startsWith(tuple._1))) {
      val result = parseExpression(string.substring(name.length), currentPrecedence)
      if (result.nonEmpty) {
        return Option.apply((
          Expressions.constructUnaryOperator(name, result.get._1),
          result.get._2
        ))
      }
    }
    Option.empty
  }

  private[this] def parseBinaryOperator(firstArgument: Option[(Expression, String)], currentPrecedence: Int): Option[(Expression, String)] = {
    if (firstArgument.isEmpty)
      return Option.empty
    for ((name, info) <- Expressions
        .operators
        .binaryOperators
        .info
        .filter((tuple) => tuple._2.precedence == currentPrecedence &&
          firstArgument.get._2.startsWith(tuple._1))) {
      if (info.associativity == Associativity.Left) {
        var currentResult = firstArgument
        while (currentResult.nonEmpty && currentResult.get._2.startsWith(name)) {
          val nextArgument = parseExpression(
            currentResult.get._2.substring(name.length),
            currentPrecedence - 1
          )
          if (nextArgument.nonEmpty) {
            currentResult =
              for {
                (cur, _) <- currentResult
                (next, newRest) <- nextArgument
              } yield (Expressions.constructBinaryOperator(name, cur, next), newRest)
          }
        }
        return currentResult
      } else {
        val secondArgument = parseExpression(
          firstArgument.get._2.substring(name.length),
          currentPrecedence
        )
        if (secondArgument.nonEmpty)
          return Option.apply((Expressions
            .constructBinaryOperator(
              name,
              firstArgument.get._1,
              secondArgument.get._1), secondArgument.get._2
          ))
      }
    }
    Option.empty
  }

  private[this] def parseExpression(string: String, currentPrecedence: Int = Expressions.operators.precedenceRange.max): Option[(Expression, String)] = {
    if (currentPrecedence == 0) {
      if (string.startsWith("(")) {
        val bracedExpression = parseExpression(string.substring(1))
        if (bracedExpression.nonEmpty && bracedExpression.get._2.startsWith(")")) {
          return bracedExpression.map((tuple) => (tuple._1, tuple._2.substring(1)))
        }
        return Option.empty
      }
      return (parseNumber(string) ++ parseFunction(string)).reduceLeftOption((a, _) => a)
    }
    val unaryParsed = parseUnaryOperator(string, currentPrecedence)
    if (unaryParsed.nonEmpty) {
      return unaryParsed
    }
    var binaryOperation = parseExpression(string, currentPrecedence - 1)
    while (true) {
      val currentOperation = parseBinaryOperator(binaryOperation, currentPrecedence)
      if (currentOperation.isEmpty)
        return binaryOperation
      binaryOperation = currentOperation
    }
    Option.empty
  }

  private[this] def parseNumber(string: String): Option[(MyNumber, String)] = {
    object doublePredicate extends ((Char) => Boolean) {
      var isIntegerPart: Boolean = true

      override def apply(c: Char): Boolean =
        if (c.isDigit) {
          true
        } else if (c == '.' && isIntegerPart) {
          isIntegerPart = false
          true
        } else {
          false
        }
    }

    val splitString = string.span(doublePredicate)

    if (splitString._1.isEmpty)
      Option.empty
    else
      try
        Option.apply((Expressions.constructNumber(splitString._1.toDouble), splitString._2))
      catch {
        case _: Throwable => Option.empty
      }
  }
}