package dev.vgerasimov.slowparse

import scala.annotation.{ tailrec, targetName }
import scala.language.postfixOps

/** Function acception an input to be parsed and returning [[POut]]. */
trait P[+A] extends (String => POut[A])

/** A [[P]]arser that returns a pair of one value and a function that can be evaluated in order to "continue" parsing.
  */
trait AndLazyThen[A, +B] extends P[(A, () => POut[B])]

/** Represents a result of parsing. */
sealed trait POut[+A]

/** Contains implementations of [[POut]]. */
object POut:

  /** Represents successful parsing result. */
  case class Success[+A](
    value: A,
    parsed: String,
    remaining: String,
    parserLabel: Option[String] = None
  ) extends POut[A]

  /** Represents failed parsing result. */
  case class Failure(
    message: String,
    parserLabel: Option[String] = None
  ) extends POut[Nothing]

  object Failure:
    def fromExpected(
      expected: String,
      got: String,
      ctx: (String, String),
      parserLabel: Option[String] = None
    ): Failure =
      Failure(
        s"""|Expected: $expected
          |Got:      $got
          |   ${if (ctx._1.isEmpty) "   " else "..."}${ctx._1}${ctx._2}${if (ctx._2.isEmpty) "   " else "..."}
          |      ${" ".repeat(ctx._1.length)}^
      """.stripMargin,
        parserLabel
      )

  def ctx(before: String = "", after: String = ""): (String, String) =
    (before.safeSlice(from = before.length - 5), after.safeSlice(until = 5))

/** Contains simple constructors for [[P]]. */
object P:

  /** Lazily calls given parser.
    *
    * Helpful for creating mutually recursive parsers.
    */
  def apply[A](parser: => P[A]): P[A] = input => parser(input)

  /** Alias for [[Parsers.char]]. */
  def apply(char: Char): P[Unit] = Parsers.char(char)

  /** Alias for [[Parsers.string]]. */
  def apply(string: String): P[Unit] = Parsers.string(string)

@scala.annotation.implicitNotFound("Cannot find sequencer for (${A}, ${B}) => ${C}")
trait Sequencer[-A, -B, +C] extends ((A, B) => C)

/** Contains postfix variants of many parser combinators in [[Parsers]]. */
extension [A](self: P[A])

  def andThen[B, C](next: P[B])(using Sequencer[A, B, C]): P[C] = Parsers.andThen(self, next)
  def andThenFlatMap[B, C](next: A => P[B])(using Sequencer[A, B, C]): P[C] = Parsers.andThenFlatMap(self, next)
  def ~ [B, C](next: P[B])(using Sequencer[A, B, C]): P[C] = Parsers.andThen(self, next)
  def ~~ [B, C](next: P[B])(using Sequencer[A, Unit, A], Sequencer[A, B, C]): P[C] = self ~ Parsers.ws0 ~ next
  def ~-~ [B, C](next: P[B])(using Sequencer[A, Unit, A], Sequencer[A, B, C]): P[C] = self ~ Parsers.ws1 ~ next
  def ~/ [B, C](next: P[B])(using Sequencer[A, B, C]): P[C] = Parsers.cut(self, next)

  def orElse[B](other: P[B]): P[A | B] = Parsers.orElse(self, other)
  def | [B](other: P[B]): P[A | B] = Parsers.orElse(self, other)

  @targetName("unaryExclamationMark") def unary_! : P[Unit] = Parsers.not(self)

  @targetName("exclamationMark") def ! : P[String] = Parsers.capture(self)
  @targetName("doubleExclamationMark") def !! : P[Unit] = Parsers.unCapture(self)
  @targetName("questionMark") def ? : P[Option[A]] = Parsers.optional(self)

  def map[B](f: A => B): P[B] = Parsers.map(self)(f)
  def flatMap[B](f: A => P[B]): P[B] = Parsers.flatMap(self)(f)
  def filter(f: A => Boolean): P[A] = Parsers.filter(self)(f)

  def rep(
    min: Int = 0,
    max: Int = Int.MaxValue,
    greedy: Boolean = true,
    sep: Option[P[Unit]] = None
  ): P[List[A]] = Parsers.rep(self)(min, max, greedy, sep)

  @targetName("plus") def + : P[List[A]] = Parsers.rep(self)(min = 1, max = Int.MaxValue, greedy = true)
  @targetName("star") def * : P[List[A]] = Parsers.rep(self)(min = 0, max = Int.MaxValue, greedy = true)

  def label(label: String): P[A] = Parsers.label(self)(label)

/** Contains basic implementations and combinators for [[P]]. */
object Parsers:
  import POut.*
  export Sequencers.given

  /** Always succeeding parser consuming no characters. */
  val success: P[Unit] = input => Success((), "", input)

  /** Always failing parser. */
  def fail[A]: P[A] = input => Failure("this parser always fails")

  /** Parses any single end-of-line character. */
  val eol: P[Unit] = anyFrom("\n\r").label("eol")

  /** Parses single tab character. */
  val tab: P[Unit] = P('\t')

  /** Parses single whitespace character. */
  val space: P[Unit] = P(' ')

  /** Parses single whitespace or tab character. */
  val s: P[Unit] = (space | tab).label("s")

  /** Parses zero or more whitespace or tab characters and drops collected value. */
  val s0: P[Unit] = s.*.!!

  /** Parses one or more whitespace or tab characters and drops collected value. */
  val s1: P[Unit] = s.+.!!

  /** Parses any single whitespace character. */
  val ws: P[Unit] = (eol | s).label("ws")

  /** Parses zero or more whitespace characters and drops collected value. */
  val ws0: P[Unit] = ws.*.!!

  /** Parses one or more whitespace characters and drops collected value. */
  val ws1: P[Unit] = ws.+.!!

  /** Parses single digit character. */
  val d: P[Unit] = fromRange('0' to '9').label("digit")

  /** Parses single digit character.
    *
    * Alias for [[Parsers.d]].
    */
  val digit: P[Unit] = d

  /** Parses single lower alpha (a-z) character. */
  val alphaLower: P[Unit] = fromRange('a' to 'z')

  /** Parses single upper alpha (A-Z) character. */
  val alphaUpper: P[Unit] = fromRange('A' to 'Z')

  /** Parses single lower or upper alpha character. */
  val alpha: P[Unit] = alphaLower | alphaUpper

  /** Parses single alpha-numeric character. */
  val alphaNum: P[Unit] = d | alpha

  /** Parser returning success only if input is empty. */
  val end: P[Unit] = input => {
    input match
      case "" => Success((), "", "")
      case x  => Failure.fromExpected(expected = "<end of line>", got = input, ctx = ctx(after = input))
  }

  /** Parses characters satisfying given condition. */
  def charsWhile(
    condition: Char => Boolean
  ): P[String] = input => {
    // TODO: Maybe I should rewrite it in a more functional style.
    var toParse = input
    var remaining = toParse
    var parsed = ""
    while (toParse.nonEmpty) {
      val char = toParse.head
      if (condition(char)) {
        parsed += char
        toParse = toParse.tail
        remaining = toParse
      } else {
        toParse = ""
      }
    }
    Success(parsed, parsed, remaining)
  }

  /** Parses all characters until some of them is presented in given string. */
  def charsUntilIn(string: String): P[String] =
    val chars = string.toSet
    charsWhile(c => !chars.contains(c))

  /** Parses all characters until end of line. */
  val charsUntilEol: P[String] = charsUntilIn("\n\r")

  /** Positive-lookahead parser, consumes no input. */
  def & [A](parser: P[A]): P[A] = input =>
    parser(input) match
      case POut.Success(v, _, _, _) => POut.Success(v, "", input)
      case f: POut.Failure          => f

  /** Parses returning failure only if input is empty. */
  val anyChar: P[Unit] = input => {
    input.safeSlice(from = 0, until = 1) match
      case "" => Failure(s"expected: <any char>, got: <end of input>")
      case x  => Success((), x, input.safeSlice(from = 1))
  }

  /** Parses end of line or end of input. */
  val eolOrEnd: P[Unit] = eol | end

  /** Parses any character from the given string. */
  def anyFrom(chars: String): P[Unit] = choice(chars.map(char)*)

  /** Parses everyting until given parser succeed. */
  def until(parser: P[?], collector: P[?] = anyChar): P[Unit] =
    unCapture(rep(!parser ~ collector)(greedy = true))

  // TODO: remove, seems completely unneeded
  def surrounded[A](
    fromParser: P[?],
    toParser: P[?],
    contentParser: P[A]
  ): P[A] = fromParser.!! ~ contentParser ~ toParser.!!

  def surrounded[A](
    surroundingParser: P[?],
    contentParser: P[A]
  ): P[A] = surrounded(surroundingParser, surroundingParser, contentParser)

  def surrounded(
    surroundingParser: P[?]
  ): P[String] = surrounded(surroundingParser, (!surroundingParser ~ anyChar.!).*).mkString

  /** Parses given character. */
  def char(char: Char): P[Unit] = input => {
    input.safeHead match
      case x if x == char.toString => Success((), char.toString, input.tail)
      case x                       => Failure.fromExpected(expected = char.toString, got = x, ctx = ctx(after = input))
  }

  /** Parses given string */
  def string(str: String): P[Unit] = input => {
    input.safeSlice(until = str.length) match
      case x if x == str => Success((), str, input.safeSlice(str.length))
      case x             => Failure(s"expected: $str, got: $x")
  }

  /** Attaches given label to the parser. */
  def label[A](parser: P[A])(string: String): P[A] = new P[A] {
    private val label = string
    override def apply(input: String): POut[A] = {
      parser(input) match
        case Success(v, parsed, remaining, _) => Success(v, parsed, remaining, Some(label))
        case Failure(message, _)              => Failure(message, Some(label))
    }
  }

  /** Applies given function to successful result of calling given parser. */
  def map[A, B](parser: P[A])(f: A => B): P[B] = input => parser(input).map(f)

  /** Applies given function returning another parser to successful result of calling given parser. */
  def flatMap[A, B](parser: P[A])(f: A => P[B]): P[B] = input => {
    parser(input) match
      case x: Failure                       => x
      case Success(v, parsed, remaining, _) => f(v)(remaining)
  }

  /** Wraps result of calling given parser into [[Option]], thus, never fails. */
  def optional[A](parser: P[A]): P[Option[A]] = input => {
    parser(input) match
      case x: Success[A]       => x.map(Some(_))
      case Failure(message, _) => Success(None, "", input)
  }

  /** Unwraps parser returning [[Option]] by failing if result is `None`. */
  def unOption[A](parser: P[Option[A]]): P[A] = map(filter(parser)(opt => opt != None))(_.get)

  /** Checks that parsed value satisfies given condition, if not - fails. */
  def filter[A](parser: P[A])(cond: A => Boolean): P[A] = input => {
    parser(input) match
      case x: Failure                      => x
      case Success(v, _, _, _) if !cond(v) => Failure(s"parsed value $v doesn't satisfy given condition")
      case x: Success[A]                   => x
  }

  /** Concatenates two given parsers. */
  def andThen[A, B, C](parser1: P[A], parser2: P[B])(using sequencer: Sequencer[A, B, C]): P[C] = input => {
    parser1(input) match
      case Success(value1, parsed1, remaining, _) =>
        parser2(remaining) match
          case Success(value2, parsed2, remaining, _) =>
            Success(sequencer(value1, value2), parsed1 + parsed2, remaining)
          case Failure(message, _) => Failure(message)
      case Failure(message, _) => Failure(message)
  }

  /** Concatenates two given parsers in a flatMap manner. */
  def andThenFlatMap[A, B, C](parser1: P[A], parser2: A => P[B])(using sequencer: Sequencer[A, B, C]): P[C] = input => {
    parser1(input) match
      case Success(value1, parsed1, remaining, _) =>
        parser2(value1)(remaining) match
          case Success(value2, parsed2, remaining, _) =>
            Success(sequencer(value1, value2), parsed1 + parsed2, remaining)
          case Failure(message, _) => Failure(message)
      case Failure(message, _) => Failure(message)
  }

  /** Concatenates given sequence of parsers. */
  def concat[A](parsers: P[A]*): P[List[A]] =
    parsers
      .map(parser => map(parser)(List(_)))
      .reduce((parser1, parser2) => andThen(parser1, parser2)(using _ ++ _))

  def choice[A](parsers: P[A]*): P[A] = parsers.reduce(orElse)
  def choice[A](parsers: Iterable[P[A]]): P[A] = parsers.reduce(orElse)

  def rep[A](
    parser: P[A]
  )(
    min: Int = 0,
    max: Int = Int.MaxValue,
    greedy: Boolean = true,
    sep: Option[P[Unit]] = None,
    condition: A => Boolean = (_: A) => true
  ): P[List[A]] =
    require(min >= 0, s"got min reps = $min; cannot be negative")
    require(min <= max, s"got min reps = $min; must be not greater than max reps = $max")
    val nextParser = sep.map(andThen(_, parser)).getOrElse(parser)
    @tailrec def iter(i: Int, parser: P[A], values: List[A], parsed: String, remaining: String): POut[List[A]] =
      if (i == max || (i == min && !greedy)) Success(values, parsed, remaining)
      else if (remaining.isEmpty)
        if (i < min) Failure(s"expected minimum $min repetions, but parsed only $i")
        else Success(values, parsed, remaining)
      else
        parser(remaining) match
          case Success(v, p, r, _) if condition(v)         => iter(i + 1, nextParser, v :: values, parsed + p, r)
          case Success(v, p, r, _) if min <= i && i <= max => Success(values, parsed, remaining)
          case _: Failure if min <= i && i <= max          => Success(values, parsed, remaining)
          // TODO: make error message more meaningful
          case _ => Failure(s"rep failed")
    (input => iter(0, parser, Nil, "", input)).map(_.reverse)

  def capture(parser: P[?]): P[String] =
    input => {
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

  def orElse[A, B](parser1: P[A], parser2: P[B]): P[A | B] = input => {
    parser1(input) match
      case Success(v, parsed, remaining, _) => Success(v, parsed, remaining)
      case f @ Failure(message, _) =>
        parser1 match
          case _: CutP[?] => f
          case _ =>
            parser2(input) match
              case Success(v, parsed, remaining, _) => Success(v, parsed, remaining)
              case x: Failure                       => x.dropLabel
  }

  def fromRange(range: scala.collection.immutable.NumericRange.Inclusive[Char]): P[Unit] = choice(range.map(char)*)

  def fromRange(range: String): P[Unit] = charRange.+(range) match
    case Success(ranges, _, _, _) =>
      choice(ranges.map { case (fromChar, toChar) => fromRange(fromChar to toChar) }*)
    case _: Failure => ignoredInput => Failure(s"cannot parse given range: $range")
  private val charRange: P[(Char, Char)] = anyChar.!.map(_.head) ~ P("-") ~ anyChar.!.map(_.head)

  def cut[A, B, C](parser1: P[A], parser2: P[B])(using sequencer: Sequencer[A, B, C]): P[C] =
    new CutP[C]:
      override def apply(input: String): POut[C] =
        parser1.andThen(parser2)(input)

  private trait CutP[+A] extends P[A]

  def andLazyThen[A, B, C](parser1: P[A], parser2: => P[B])(using
    sequencer: Sequencer[A, B, C]
  ): AndLazyThen[A, C] = input => {
    parser1(input) match
      case Success(value1, parsed1, remaining, _) =>
        Success(
          (value1, () => parser2(remaining).map(sequencer(value1, _))),
          parsed1,
          remaining
        )
      case Failure(message, _) => Failure(message)
  }

  def mapAndLazyThen[A, B1, B2](
    andLazyThen: AndLazyThen[A, B1]
  )(f: B1 => B2): AndLazyThen[A, B2] =
    input => {
      andLazyThen(input) match
        case Success((v, next), parsed, remaining, _) =>
          Success((v, () => next().map(f)), parsed, remaining)
        case f: Failure => f
    }

  def evalAndLazyThen[A, B](andLazyThen: AndLazyThen[A, B]): P[B] = input => {
    andLazyThen(input) match
      case Success((v, next), parsed1, _, _) =>
        next() match
          case Success(value, parsed2, remaining, _) => Success(value, parsed1 + parsed2, remaining)
          case f: Failure                            => f
      case f: Failure => f
  }

end Parsers

extension (self: P[List[?]]) def mkString: P[String] = self.map(_.mkString)

extension (self: String)
  private[slowparse] def safeHead: String = safeSlice(0, 1)
  private[slowparse] def safeSlice(from: Int = 0, until: Int = Int.MaxValue): String = self match
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
