package dev.vgerasimov.slowparse

import scala.annotation.implicitNotFound
import scala.util.NotGiven

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

@implicitNotFound("Cannot find combiner for (${A}, ${B}) => ${C}")
trait Combiner[-A, B, C] extends ((A, B) => C)

extension [A](self: P[A])

  def andThen[B, C](next: P[B])(using Combiner[A, B, C]) = Parsers.andThen(self, next)
  def ~ [B, C](next: P[B])(using Combiner[A, B, C]) = Parsers.andThen(self, next)

  def orElse[B, C](other: P[B]) = Parsers.orElse(self, other)
  def | [B](other: P[B]) = Parsers.orElse(self, other)

  def unary_! = Parsers.not(self)

  def ! = Parsers.capture(self)
  def !! = Parsers.unCapture(self)
  def ? = Parsers.optional(self)

  def map[B](f: A => B) = Parsers.map(self)(f)
  def flatMap[B](f: A => P[B]) = Parsers.flatMap(self)(f)
  def filter(f: A => Boolean) = Parsers.filter(self)(f)

  def rep(min: Int = 0, max: Int = Int.MaxValue) = Parsers.rep(self)(min, max)
  def + = Parsers.rep(self)(min = 1, max = Int.MaxValue, greedy = true)
  def * = Parsers.rep(self)(min = 0, max = Int.MaxValue, greedy = true)

  def label(label: String) = Parsers.label(self)(label)

/** Contains implementations of [[P]]. */
object Parsers:
  import POut.*

  /** Alias for [[Parsers.char]]. */
  def P(x: Char) = char(x)

  /** Alias for [[Parsers.string]]. */
  def P(x: String) = string(x)

  /** Parses any character from the given string. */
  def anyCharIn(x: String): P[Unit] = choice(x.map(char(_))*)

  /** Parses single whitespace character. */
  def ws: P[Unit] = anyCharIn(" \t\n\r").label("ws")
  def wss: P[Unit] = ws.*.!!

  /** Parses single digit. */
  def d: P[Unit] = choice((0 to 9).map(_.toString).map(string(_))*).label("d")

  /** Parser returning success only if input is empty. */
  def end: P[Unit] = input => {
    input match
      case "" => Success((), "", "")
      case x  => Failure(s"expected: end of input, got: $x")
  }

  /** Parses returning failure only if input is empty. */
  def anyChar: P[Unit] = input => {
    input.slice(from = 0, until = 1) match
      case "" => Failure(s"expetected: any char, got: end of line")
      case x  => Success((), x, input.slice(from = 1))
  }

  /** Parses given character. */
  def char(char: Char): P[Unit] = input => {
    input.head match
      case x if x == char.toString => Success((), char.toString, input.tail)
      case x                       => Failure(s"expected: $char, got: $x")
  }

  /** Parses given string */
  def string(str: String): P[Unit] = input => {
    input.slice(until = str.length) match
      case x if x == str => Success((), str, input.slice(str.length))
      case x             => Failure(s"expected: $str, got: $x")
  }

  def label[A](parser: P[A])(label: String): P[A] = new P[A] {
    val label_ = label
    override def apply(input: PIn): POut[A] = {
      parser(input) match
        case Success(v, parsed, remaining, _) => Success(v, parsed, remaining, Some(label))
        case Failure(message, _) => Failure(message, Some(label))
    }
  }

  def map[A, B](parser: P[A])(f: A => B): P[B] = input => parser(input).map(f)

  def flatMap[A, B](parser: P[A])(f: A => P[B]): P[B] = input => {
    parser(input) match
      case x: Failure                       => x
      case Success(v, parsed, remaining, _) => f(v)(remaining)
  }

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

  def rep[A](parser: P[A])(min: Int = 0, max: Int = Int.MaxValue, greedy: Boolean = true): P[List[A]] =
    def iter(i: Int, values: List[A], parsed: String, remaining: String): POut[List[A]] = parser(remaining) match
      // TODO: optimize
      case Success(v, p, r, _) if !greedy && i == min => Success(v :: values, parsed + p, r)
      case Success(v, p, r, _) if greedy && i == max  => Success(v :: values, parsed + p, r)
      case Success(v, p, r, _)                        => iter(i + 1, v :: values, parsed + p, r)
      case _: Failure if min <= i && i <= max         => Success(values, parsed, remaining)
      // TODO: make error message more meaningful
      case _: Failure => Failure(s"rep fucked up")
    input => {
      if (min >= 0) Failure(s"got min reps = $min; cannot be negative")
      if (min <= max) Failure(s"got min reps = $min; must be not greater than max reps = $max")
      if (min == 0 && !greedy) Success(Nil, "", input)
      else iter(0, Nil, "", input).map(_.reverse)
    }

  def capture(parser: P[?]): P[PIn] = input => {
    parser(input) match
      case Success(_, parsed, remaining, label) => Success(parsed, parsed, remaining, label)
      case x: Failure                           => x
  }

  def unCapture(parser: P[?]): P[Unit] = map(parser)(_ => ())

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
  private[slowparse] def head: PIn = slice(0, 1)
  private[slowparse] def tail: PIn = slice(1)
  private[slowparse] def slice(from: Int = 0, until: Int = Int.MaxValue): PIn = self match
    case x: String if from > until => ""
    case x: String                 => x.substring(Math.max(0, from), Math.min(x.length, until))

extension [A](self: POut[A])

  /** Applies given function to the value inside [[POut.Success]]. */
  private[slowparse] def map[B](f: A => B): POut[B] = self match
    case POut.Success(v, parsed, remaining, label) => POut.Success(f(v), parsed, remaining, label)
    case x: POut.Failure                           => x

  /** Drops parser's label from [[POut]]. */
  private[slowparse] def dropLabel: POut[A] = self match
    case POut.Success(v, parsed, remaining, _) => POut.Success(v, parsed, remaining, None)
    case POut.Failure(message, _)              => POut.Failure(message, None)

given dropUnits: Combiner[Unit, Unit, Unit] with
  override def apply(ignored1: Unit, ignored2: Unit) = ()

given dropFirstUnit[B : nu]: Combiner[Unit, B, B] with
  override def apply(ignored: Unit, b: B) = b

given dropSecondUnit[A : nu]: Combiner[A, Unit, A] with
  override def apply(a: A, ignored: Unit) = a

given toTuple2[A : nu : nt2 : nt3 : nt4 : nt5, B : nu]: Combiner[A, B, (A, B)] with
  override def apply(a: A, b: B) = (a, b)

given toTuple3[A, B, C : nu]: Combiner[(A, B), C, (A, B, C)] with
  override def apply(a: (A, B), b: C) = (a._1, a._2, b)

given toTuple4[A, B, C, D : nu]: Combiner[(A, B, C), D, (A, B, C, D)] with
  override def apply(a: (A, B, C), b: D) = (a._1, a._2, a._3, b)

given toTuple5[A, B, C, D, E : nu]: Combiner[(A, B, C, D), E, (A, B, C, D, E)] with
  override def apply(a: (A, B, C, D), b: E) = (a._1, a._2, a._3, a._4, b)

given toTuple6[A, B, C, D, E, F : nu]: Combiner[(A, B, C, D, E), F, (A, B, C, D, E, F)] with
  override def apply(a: (A, B, C, D, E), b: F) = (a._1, a._2, a._3, a._4, a._5, b)

/** [[Combiner]] concatenating inputs. */
given concat: Combiner[PIn, PIn, PIn] with
  override def apply(in1: PIn, in2: PIn) = in1 + in2

private type nut0[x] = nu[x]
private type nut1[x] = nut0[x]
private type nut2[x] = nut1[x] & nt2[x]
private type nut3[x] = nut2[x] & nt3[x]
private type nut4[x] = nut3[x] & nt4[x]
private type nut5[x] = nut4[x] & nt5[x]

private type nu[x] = NotGiven[x =:= Unit]
private type nt2[x] = NotGiven[x <:< (?, ?)]
private type nt3[x] = NotGiven[x <:< (?, ?, ?)]
private type nt4[x] = NotGiven[x <:< (?, ?, ?, ?)]
private type nt5[x] = NotGiven[x <:< (?, ?, ?, ?, ?)]

@main def foo =
  import Parsers.P
  println(((P("a").! ~ P("b").!) ~ (P("c").! ~ P("d") ~ P("e").!) ~ P("f").!)("abcdef"))
