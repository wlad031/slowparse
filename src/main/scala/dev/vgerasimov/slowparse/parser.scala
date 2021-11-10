package dev.vgerasimov.slowparse

/** Represents a result of parsing. */
sealed trait ParsingResult[+A]

/** Contains implementations of [[ParsingResult]]. */
object ParsingResult:
  /** Represents successful parsing result. */
  case class Success[A](value: A, remaining: String) extends ParsingResult[A]
  /** Represents failed parsing result. */
  case class Failure(message: String) extends ParsingResult[Nothing]

/** Function acception an input to be parsed and returning [[ParsingResult]]. */
trait Parser[+A] extends (String => ParsingResult[A])

/** Contains implementations of [[Parser]]. */
object Parser:

  /** Parses single given character. */
  def char(c: Char): Parser[Char] = input => {
    input(0) match
      case x if x == c => ParsingResult.Success(c, input.substring(1))
      case x => ParsingResult.Failure(s"expected: $c, got: $x")
  }

  def andThen[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = input => {
    p1.apply(input) match
      case ParsingResult.Success(value1, remaining) =>
        p2.apply(remaining) match
          case ParsingResult.Success(value2, remaining) =>
            ParsingResult.Success((value1, value2), remaining)
          case ParsingResult.Failure(message) =>
            ParsingResult.Failure(message)
      case ParsingResult.Failure(message) => ParsingResult.Failure(message)
  }
