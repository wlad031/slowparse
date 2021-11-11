package dev.vgerasimov.slowparse

/** Represents a type of input for parsing. */
type PIn = String

extension (self: PIn)
  def toString: String = self match
    case x: String => x

  def length: Int = self match
    case x: String => x.length

  def window(startInclusive: Int = 0, endExclusive: Int = Int.MaxValue): PIn = self match
    case x: String if (startInclusive > endExclusive) => ""
    case x: String => x.substring(Math.max(0, startInclusive), Math.min(x.length, endExclusive))

/** Represents a result of parsing. */
sealed trait POut[+A]

/** Contains implementations of [[POut]]. */
object POut:
  /** Represents successful parsing result. */
  case class Success[+A](value: A, parsed: PIn, remaining: PIn, parserLabel: Option[String] = None) extends POut[A]
  /** Represents failed parsing result. */
  case class Failure(message: String, parserLabel: Option[String] = None) extends POut[Nothing]

  extension [A](self: Success[A])
    def map[B](f: A => B): Success[B] = Success(f(self.value), self.parsed, self.remaining, self.parserLabel)

/** Function acception an input to be parsed and returning [[POut]]. */
trait P[+A] extends (PIn => POut[A])

trait Combiner[-A, -B, +C] extends ((A, B) => C)

given dropFirstUnit[A]: Combiner[Unit, A, A] with  
  override def apply(ignored: Unit, a: A) = a

given Combiner[PIn, PIn, PIn] with
  override def apply(in1: PIn, in2: PIn) = s"$in1$in2" // TODO: optimize

extension [A](self: P[A])

  def andThen[B, C](next: P[B])(using Combiner[A, B, C]) = Parsers.andThen(self, next)
  def  ~ [B, C](next: P[B])(using Combiner[A, B, C]) = Parsers.andThen(self, next)

  def ! = Parsers.capture(self)
  def ? = Parsers.optional(self)

  def map[B](f: A => B) = Parsers.map(self)(f)

  def label(label: String) = Parsers.label(self)(label)

/** Contains implementations of [[P]]. */
object Parsers:
  import POut.*

  def label[A](parser: P[A])(label: String): P[A] = input => {
    parser(input) match
      case Success(v, parsed, remaining, _) => Success(v, parsed, remaining, Some(label))
      case Failure(message, _) => Failure(message, Some(label))
  }

  def map[A, B](parser: P[A])(f: A => B): P[B] = input => {
    parser(input) match
      case x : Success[A] => x.map(f)
      case x : Failure => x
  }

  /** Parses single given character. */
  def char(char: Char): P[Unit] = input => {
    input.window(startInclusive = 0, endExclusive = 1) match
      case x if x == char.toString => Success((), char.toString, input.window(startInclusive = 1))
      case x => Failure(s"expected: $char, got: $x")
  }

  def string(str: String): P[Unit] = input => {
    input.window(startInclusive = 0, endExclusive = str.length) match
      case x if x == str => Success((), str, input.window(startInclusive = str.length))
      case x => Failure(s"expected: $char, got: $x")
  }

  def optional[A](parser: P[A]): P[Option[A]] = input => {
    parser(input) match
      case x : Success[A] => x.map(Some(_))
      case Failure(message, label) => Success(None, "", input, label)
  }

  def seq[A](parsers: P[A]*): P[List[A]] =
    parsers
      .map(parser => map(parser)(List(_)))
      .reduce((parser1, parser2) => Parsers.andThen(parser1, parser2)(using _ ++ _))
  
  def rep[A](min: Int = 0, max: Int = Int.MaxValue)(parser: P[A]): P[List[A]] = ???

  def capture(parser: P[?]): P[PIn] = input => {
    parser(input) match
      case Success(_, parsed, remaining, label) => Success(parsed, parsed, remaining, label)
      case x : Failure => x
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
