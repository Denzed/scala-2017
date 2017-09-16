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
  def parse(string: String): Option[Expression] =
    parseExpression(string).filter(_._2.isEmpty).map(_._1)

  private[this] def parseFunctionArguments(left: Int, string: String): Option[(Seq[Expression], String)] =
    if (left == 0)
      Option.apply((Seq(), string))
    else
      for {
        (firstArg, rest1) <- parseExpression(string)
          .filter(_._2.startsWith(if (left > 1) "," else ")"))
        (otherArgs, rest2) <- parseFunctionArguments(left - 1, rest1.substring(1))
      } yield (otherArgs.+:(firstArg), rest2)


  private[this] def isValidFunctionBeginning(string: String, name: String): Boolean =
    string.startsWith(name ++ "(")

  private[this] def parseFunction(string: String): Option[(Expression, String)] =
    (for {
      (name, info) <- Expressions
        .mathFunctions
        .info
        .filterKeys((name) => isValidFunctionBeginning(string, name))
        .toList
      (args, rest) <- parseFunctionArguments(
        info.argumentCount,
        string.substring(name.length + 1))
    } yield (Expressions.constructMathFunction(name, args), rest))
      .reduceLeftOption((res, _) => res)


  private[this] def parseUnaryOperator(string: String, currentPrecedence: Int): Option[(Expression, String)] =
    (for {
      (name, _) <- Expressions
        .operators
        .unaryOperators
        .info
        .filter((tuple) => tuple._2.precedence == currentPrecedence && string.startsWith(tuple._1))
        .toList
      (underlying, rest) <- parseExpression(string.substring(name.length), currentPrecedence)
    } yield (Expressions.constructUnaryOperator(name, underlying), rest))
      .reduceLeftOption((res, _) => res)

  private[this] def parseLeftBinaryOperatorHelper(parsed: Option[(Expression, String)], currentPrecedence: Int, name: String): Option[(Expression, String)] =
    for {
      (cur, rest) <- parsed
      (next, newRest) <- parseExpression(rest.substring(name.length), currentPrecedence - 1)
    } yield (Expressions.constructBinaryOperator(name, cur, next), newRest)

  private[this] def parseBinaryOperator(firstArgumentOption: Option[(Expression, String)], currentPrecedence: Int): Option[(Expression, String)] =
    (for {
      (firstArgument, rest) <- firstArgumentOption.toSeq
      (name, info) <- Expressions
        .operators
        .binaryOperators
        .info
        .filter((tuple) => tuple._2.precedence == currentPrecedence &&
          rest.startsWith(tuple._1))
        .toList
      result <- info.associativity match {
        case Associativity.Left => parseLeftBinaryOperatorHelper(firstArgumentOption, currentPrecedence, name)
        case Associativity.Right => parseExpression(rest.substring (name.length), currentPrecedence)
          .map((secondArgumentTuple) => (Expressions
            .constructBinaryOperator(name, firstArgument, secondArgumentTuple._1), secondArgumentTuple._2
          ))
      }
    } yield result)
      .reduceLeftOption((res, _) => res)

  private[this] def parseBracedExpression(string: String): Option[(Expression, String)] =
    if (string.startsWith("("))
      parseExpression(string.substring(1))
        .filter(_._2.startsWith(")"))
        .map((tuple) => (tuple._1, tuple._2.substring(1)))
    else
      Option.empty

  private[this] def parseExpressionWithBinaryOperators(string: String, currentPrecedence: Int): Option[(Expression, String)] = {
    object helper extends ((Option[(Expression, String)]) => Option[(Expression, String)]) {
      override def apply(binaryOperation: Option[(Expression, String)]): Option[(Expression, String)] =
        binaryOperation match {
          case Some(_) => (apply(parseBinaryOperator(binaryOperation, currentPrecedence)) ++ binaryOperation)
            .reduceLeftOption((res, _) => res)
          case _ => Option.empty
        }
    }

    helper(parseExpression(string, currentPrecedence - 1))
  }

  private[this] def parseExpression(string: String, currentPrecedence: Int = Expressions.operators.precedenceRange.max): Option[(Expression, String)] =
    (if (currentPrecedence == 0)
      parseNumber(string) ++
        parseBracedExpression(string) ++
        parseFunction(string)
    else
      parseUnaryOperator(string, currentPrecedence) ++
        parseExpressionWithBinaryOperators(string, currentPrecedence)
    ).reduceLeftOption((res, _) => res)

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

    try {
      val splitString = string.span(doublePredicate)
      Option.apply((Expressions.constructNumber(splitString._1.toDouble), splitString._2))
    } catch {
      case _: NumberFormatException => Option.empty
    }
  }
}