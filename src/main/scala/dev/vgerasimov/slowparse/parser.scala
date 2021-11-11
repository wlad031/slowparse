package dev.vgerasimov.slowparse

/** Represents a type of input for parsing. */
type PIn = String

/** Represents a result of parsing. */
sealed trait POut[+A]

/** Contains implementations of [[POut]]. */
object POut:
  /** Represents successful parsing result. */
  case class Success[+A](value: A, parsed: PIn, remaining: PIn, parserLabel: Option[String] = None) extends POut[A]

  /** Represents failed parsing result. */
  case class Failure(message: String, parserLabel: Option[String] = None) extends POut[Nothing]

/** Function acception an input to be parsed and returning [[POut]]. */
trait P[+A] extends (PIn => POut[A])

trait Combiner[-A, -B, +C] extends ((A, B) => C)

extension [A](self: P[A])

  def andThen[B, C](next: P[B])(using Combiner[A, B, C]) = Parsers.andThen(self, next)
  def ~ [B, C](next: P[B])(using Combiner[A, B, C]) = Parsers.andThen(self, next)

  def orElse[B, C](other: P[B]) = Parsers.orElse(self, other)
  def | [B](other: P[B]) = Parsers.orElse(self, other)

  def unary_! = Parsers.not(self)

  def ! = Parsers.capture(self)
  def ? = Parsers.optional(self)

  def map[B](f: A => B) = Parsers.map(self)(f)
  def filter(f: A => Boolean) = Parsers.filter(self)(f)

  def label(label: String) = Parsers.label(self)(label)

/** Contains implementations of [[P]]. */
object Parsers:
  import POut.*

  /** Alias for [[Parsers.char]]. */
  def P(x: Char) = char(x)

  /** Alias for [[Parsers.string]]. */
  def P(x: String) = string(x)

  /** Parses any character from the given string. */
  def anyCharIn(x: String) = choice(x.map(char(_))*)

  /** Parses single whitespace character. */
  def ws = anyCharIn(" \t\n\r").label("ws")

  /** Parses single digit. */
  def d = choice((0 to 9).map(_.toString).map(string(_))*).label("d")

  /** Parser returning success only if input is empty. */
  def end: P[Unit] = input => {
    input match
      case "" => Success((), "", "")
      case x  => Failure(s"expected: end of input, got: $x")
  }

  /** Parses returning failure only if input is empty. */
  def anyChar: P[Unit] = input => {
    input.window(startInclusive = 0, endExclusive = 1) match
      case "" => Failure(s"expetected: any char, got: end of line")
      case x  => Success((), x, input.window(startInclusive = 1))
  }

  /** Parses given character. */
  def char(char: Char): P[Unit] = input => {
    input.window(startInclusive = 0, endExclusive = 1) match
      case x if x == char.toString => Success((), char.toString, input.window(startInclusive = 1))
      case x                       => Failure(s"expected: $char, got: $x")
  }

  /** Parses given string */
  def string(str: String): P[Unit] = input => {
    input.window(startInclusive = 0, endExclusive = str.length) match
      case x if x == str => Success((), str, input.window(startInclusive = str.length))
      case x             => Failure(s"expected: $char, got: $x")
  }

  def label[A](parser: P[A])(label: String): P[A] = input => {
    parser(input) match
      case Success(v, parsed, remaining, _) => Success(v, parsed, remaining, Some(label))
      case Failure(message, _)              => Failure(message, Some(label))
  }

  def map[A, B](parser: P[A])(f: A => B): P[B] = input => parser(input).map(f)

  def optional[A](parser: P[A]): P[Option[A]] = input => {
    parser(input) match
      case x: Success[A]       => x.map(Some(_))
      case Failure(message, _) => Success(None, "", input)
  }

  def filter[A](parser: P[A])(cond: A => Boolean): P[A] = input => {
    parser(input) match
      case x: Failure                      => x
      case Success(v, _, _, _) if !cond(v) => Failure(s"parsed value $v doesn't satisfy given condition")
      case x: Success[A]                   => x
  }

  def unOption[A](parser: P[Option[A]]): P[A] = map(filter(parser)(opt => opt != None))(_.get)

  def seq[A](parsers: P[A]*): P[List[A]] =
    parsers
      .map(parser => map(parser)(List(_)))
      .reduce((parser1, parser2) => andThen(parser1, parser2)(using _ ++ _))

  def choice[A](parsers: P[A]*): P[A] = parsers.reduce(orElse)

  def rep[A](min: Int = 0, max: Int = Int.MaxValue)(parser: P[A]): P[List[A]] = ???

  def capture(parser: P[?]): P[PIn] = input => {
    parser(input) match
      case Success(_, parsed, remaining, label) => Success(parsed, parsed, remaining, label)
      case x: Failure                           => x
  }

  def not(parser: P[?]): P[Unit] = input => {
    parser(input) match
      case Success(v, _, _, _) => Failure(s"unexpected: $v")
      case _: Failure          => Success((), "", input)
  }

  def andThen[A, B, C](parser1: P[A], parser2: P[B])(using combiner: Combiner[A, B, C]): P[C] = input => {
    parser1(input) match
      case Success(value1, parsed1, remaining, _) =>
        parser2(remaining) match
          case Success(value2, parsed2, remaining, _) =>
            Success(combiner(value1, value2), parsed1 + parsed2, remaining)
          case Failure(message, _) => Failure(message)
      case Failure(message, _) => Failure(message)
  }

  def orElse[A, B](parser1: P[A], parser2: P[B]): P[A | B] = input => {
    parser1(input) match
      case Success(v, parsed, remaining, _) => Success(v, parsed, remaining)
      case Failure(message, _) =>
        parser2(input) match
          case Success(v, parsed, remaining, _) => Success(v, parsed, remaining)
          case x: Failure                       => x.dropLabel
  }

extension (self: PIn)
  private[slowparse] def window(startInclusive: Int = 0, endExclusive: Int = Int.MaxValue): PIn = self match
    case x: String if startInclusive > endExclusive => ""
    case x: String => x.substring(Math.max(0, startInclusive), Math.min(x.length, endExclusive))

extension [A](self: POut[A])

  /** Applies given function to the value inside [[POut.Success]]. */
  private[slowparse] def map[B](f: A => B): POut[B] = self match
    case POut.Success(v, parsed, remaining, label) => POut.Success(f(v), parsed, remaining, label)
    case x: POut.Failure                           => x

  /** Drops parser's label from [[POut]]. */
  private[slowparse] def dropLabel: POut[A] = self match
    case POut.Success(v, parsed, remaining, _) => POut.Success(v, parsed, remaining, None)
    case POut.Failure(message, _)              => POut.Failure(message, None)

/** [[Combiner]] dropping first unit input. */
given dropFirstUnit[A]: Combiner[Unit, A, A] with
  override def apply(ignored: Unit, a: A) = a

/** [[Combiner]] concatenating inputs. */
given Combiner[PIn, PIn, PIn] with
  override def apply(in1: PIn, in2: PIn) = in1 + in2
