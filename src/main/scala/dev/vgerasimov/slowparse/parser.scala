package dev.vgerasimov.slowparse

import scala.annotation.{ tailrec, targetName }
import scala.language.postfixOps

/** Function acception an input to be parsed and returning [[POut]]. */
trait P[+A] extends (String => POut[A]):
  private[slowparse] def run(source: Source, offset: Int): InternalOut[A] =
    apply(source.value.substring(offset)) match
      case POut.Success(value, parsed, remaining, label) =>
        InternalSuccess(value, offset, source.value.length - remaining.length, label)
      case POut.Failure(message, label) => InternalFailure(message, label)

/** A [[P]]arser that returns a pair of one value and a function that can be evaluated in order to "continue" parsing.
  */
trait AndLazyThen[A, +B] extends P[(A, () => POut[B])]

/** Represents a result of parsing. */
sealed trait POut[+A]

/** Contains implementations of [[POut]]. */
private[slowparse] final case class Source(value: String)

private[slowparse] sealed trait InternalOut[+A]
private[slowparse] final case class InternalSuccess[+A](
  value: A,
  start: Int,
  end: Int,
  label: Option[String] = None
) extends InternalOut[A]
private[slowparse] final case class InternalFailure(
  message: String,
  label: Option[String] = None,
  committed: Boolean = false
) extends InternalOut[Nothing]

private[slowparse] trait InternalParser[+A]:
  def run(source: Source, offset: Int): InternalOut[A]

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
          |   ${if ctx._1.isEmpty then "   " else "..."}${ctx._1}${ctx._2}${if ctx._2.isEmpty then "   " else "..."}
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
  def apply[A](parser: => P[A]): P[A] = new P[A]:
    override def run(source: Source, offset: Int): InternalOut[A] = parser.run(source, offset)
    override def apply(input: String): POut[A] = parser(input)

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

  private def public[A](parser: InternalParser[A]): P[A] = new P[A]:
    override def run(source: Source, offset: Int): InternalOut[A] = parser.run(source, offset)
    override def apply(input: String): POut[A] =
      parser.run(Source(input), 0) match
        case InternalSuccess(value, start, end, label) =>
          Success(value, input.substring(start, end), input.substring(end), label)
        case InternalFailure(message, label, _) => Failure(message, label)

  private def internal[A](f: (Source, Int) => InternalOut[A]): InternalParser[A] =
    new InternalParser[A]:
      override def run(source: Source, offset: Int): InternalOut[A] = f(source, offset)

  private def asInternal[A](parser: P[A]): InternalParser[A] = internal { (source, offset) =>
    parser(source.value.substring(offset)) match
      case Success(value, parsed, remaining, label) =>
        InternalSuccess(value, offset, source.value.length - remaining.length, label)
      case Failure(message, label) => InternalFailure(message, label)
  }

  /** Always succeeding parser consuming no characters. */
  val success: P[Unit] = public(internal((_, offset) => InternalSuccess((), offset, offset)))

  /** Always failing parser. */
  def fail[A]: P[A] = public(internal((_, _) => InternalFailure("this parser always fails")))

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
  val end: P[Unit] = public(internal { (source, offset) =>
    if offset == source.value.length then InternalSuccess((), offset, offset)
    else InternalFailure("expected: <end of line>")
  })

  /** Parses characters satisfying given condition. */
  def charsWhile(
    condition: Char => Boolean
  ): P[String] = public(internal { (source, offset) =>
    var end = offset
    while end < source.value.length && condition(source.value.charAt(end)) do end += 1
    InternalSuccess(source.value.substring(offset, end), offset, end)
  })

  /** Parses all characters until some of them is presented in given string. */
  def charsUntilIn(string: String): P[String] =
    val chars = string.toSet
    charsWhile(c => !chars.contains(c))

  /** Parses all characters until end of line. */
  val charsUntilEol: P[String] = charsUntilIn("\n\r")

  /** Positive-lookahead parser, consumes no input. */
  def & [A](parser: P[A]): P[A] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case InternalSuccess(value, _, _, label) => InternalSuccess(value, offset, offset, label)
      case failure: InternalFailure            => failure
  })

  /** Parses returning failure only if input is empty. */
  val anyChar: P[Unit] = public(internal { (source, offset) =>
    if offset >= source.value.length then InternalFailure("expected: <any char>, got: <end of input>")
    else InternalSuccess((), offset, offset + 1)
  })

  /** Parses end of line or end of input. */
  val eolOrEnd: P[Unit] = eol | end

  /** Parses any character from the given string. */
  def anyFrom(chars: String): P[Unit] =
    if chars.isEmpty then fail
    else choice(chars.map(char)*)

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
  def char(char: Char): P[Unit] =
    val failure = InternalFailure(s"expected: $char")
    public(internal { (source, offset) =>
      if offset < source.value.length && source.value.charAt(offset) == char then
        InternalSuccess((), offset, offset + 1)
      else failure
    })

  /** Parses given string */
  def string(str: String): P[Unit] =
    val failure = InternalFailure(s"expected: $str")
    public(internal { (source, offset) =>
      if source.value.startsWith(str, offset) then InternalSuccess((), offset, offset + str.length)
      else failure
    })

  /** Attaches given label to the parser. */
  def label[A](parser: P[A])(string: String): P[A] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case InternalSuccess(value, start, end, _) => InternalSuccess(value, start, end, Some(string))
      case InternalFailure(message, _, _)        => InternalFailure(message, Some(string))
  })

  /** Applies given function to successful result of calling given parser. */
  def map[A, B](parser: P[A])(f: A => B): P[B] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case InternalSuccess(value, start, end, label) => InternalSuccess(f(value), start, end, label)
      case failure: InternalFailure                  => failure
  })

  /** Applies given function returning another parser to successful result of calling given parser. */
  def flatMap[A, B](parser: P[A])(f: A => P[B]): P[B] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case InternalSuccess(value, start, end, _) =>
        f(value).run(source, end) match
          case InternalSuccess(nextValue, _, nextEnd, label) => InternalSuccess(nextValue, start, nextEnd, label)
          case failure: InternalFailure                      => failure
      case failure: InternalFailure => failure
  })

  /** Wraps result of calling given parser into [[Option]], thus, never fails. */
  def optional[A](parser: P[A]): P[Option[A]] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case InternalSuccess(value, start, end, label) => InternalSuccess(Some(value), start, end, label)
      case _: InternalFailure                        => InternalSuccess(None, offset, offset)
  })

  /** Unwraps parser returning [[Option]] by failing if result is `None`. */
  def unOption[A](parser: P[Option[A]]): P[A] = map(filter(parser)(opt => opt != None))(_.get)

  /** Checks that parsed value satisfies given condition, if not - fails. */
  def filter[A](parser: P[A])(cond: A => Boolean): P[A] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case success @ InternalSuccess(value, _, _, _) if cond(value) => success
      case InternalSuccess(value, _, _, _) => InternalFailure(s"parsed value $value doesn't satisfy given condition")
      case failure: InternalFailure        => failure
  })

  /** Concatenates two given parsers. */
  def andThen[A, B, C](parser1: P[A], parser2: P[B])(using sequencer: Sequencer[A, B, C]): P[C] =
    public(internal { (source, offset) =>
      parser1.run(source, offset) match
        case InternalSuccess(value1, start, end, _) =>
          parser2.run(source, end) match
            case InternalSuccess(value2, _, next, _) => InternalSuccess(sequencer(value1, value2), start, next)
            case InternalFailure(message, label, _)  => InternalFailure(message, label)
        case InternalFailure(message, label, _) => InternalFailure(message, label)
    })

  /** Concatenates two given parsers in a flatMap manner. */
  def andThenFlatMap[A, B, C](parser1: P[A], parser2: A => P[B])(using sequencer: Sequencer[A, B, C]): P[C] =
    public(internal { (source, offset) =>
      parser1.run(source, offset) match
        case InternalSuccess(value1, start, end, _) =>
          parser2(value1).run(source, end) match
            case InternalSuccess(value2, _, next, _) => InternalSuccess(sequencer(value1, value2), start, next)
            case InternalFailure(message, label, _)  => InternalFailure(message, label)
        case InternalFailure(message, label, _) => InternalFailure(message, label)
    })

  /** Concatenates given sequence of parsers. */
  def concat[A](parsers: P[A]*): P[List[A]] =
    parsers
      .map(parser => map(parser)(List(_)))
      .reduce((parser1, parser2) => andThen(parser1, parser2)(using _ ++ _))

  def choice[A](parsers: P[A]*): P[A] = parsers.reduceOption(orElse).getOrElse(fail)
  def choice[A](parsers: Iterable[P[A]]): P[A] = parsers.reduceOption(orElse).getOrElse(fail)

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
    public(internal { (source, initialOffset) =>
      @tailrec def iter(i: Int, current: P[A], values: List[A], offset: Int): InternalOut[List[A]] =
        if i == max || (i == min && !greedy) then InternalSuccess(values.reverse, initialOffset, offset)
        else if offset == source.value.length then
          if i < min then InternalFailure(s"expected minimum $min repetions, but parsed only $i")
          else InternalSuccess(values.reverse, initialOffset, offset)
        else
          current.run(source, offset) match
            case InternalSuccess(value, _, next, _) if next == offset && condition(value) =>
              InternalFailure("repetition parser consumed no input")
            case InternalSuccess(value, _, next, _) if condition(value) =>
              iter(i + 1, nextParser, value :: values, next)
            case _: InternalSuccess[?] if min <= i && i <= max =>
              InternalSuccess(values.reverse, initialOffset, offset)
            case _: InternalFailure if min <= i && i <= max =>
              InternalSuccess(values.reverse, initialOffset, offset)
            case _ => InternalFailure("rep failed")
      iter(0, parser, Nil, initialOffset)
    })

  def capture(parser: P[?]): P[String] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case InternalSuccess(_, start, end, label) =>
        InternalSuccess(source.value.substring(start, end), start, end, label)
      case failure: InternalFailure => failure
  })

  def unCapture(parser: P[?]): P[Unit] = map(parser)(_ => ())

  def not(parser: P[?]): P[Unit] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case InternalSuccess(value, _, _, _) => InternalFailure(s"unexpected: $value")
      case _: InternalFailure              => InternalSuccess((), offset, offset)
  })

  def orElse[A, B](parser1: P[A], parser2: P[B]): P[A | B] = public(internal { (source, offset) =>
    parser1.run(source, offset) match
      case success: InternalSuccess[A]                   => success
      case failure: InternalFailure if failure.committed => failure
      case _: InternalFailure =>
        parser2.run(source, offset) match
          case success: InternalSuccess[B] => success
          case failure: InternalFailure    => InternalFailure(failure.message, failure.label, failure.committed)
  })

  def fromRange(range: scala.collection.immutable.NumericRange.Inclusive[Char]): P[Unit] = choice(range.map(char)*)

  def fromRange(range: String): P[Unit] = charRange.+(range) match
    case Success(ranges, _, _, _) =>
      choice(ranges.map { case (fromChar, toChar) => fromRange(fromChar to toChar) }*)
    case _: Failure => ignoredInput => Failure(s"cannot parse given range: $range")
  private val charRange: P[(Char, Char)] = anyChar.!.map(_.head) ~ P("-") ~ anyChar.!.map(_.head)

  def cut[A, B, C](parser1: P[A], parser2: P[B])(using sequencer: Sequencer[A, B, C]): P[C] =
    public(internal { (source, offset) =>
      parser1.run(source, offset) match
        case InternalSuccess(value1, start, end, _) =>
          parser2.run(source, end) match
            case InternalSuccess(value2, _, next, label) =>
              InternalSuccess(sequencer(value1, value2), start, next, label)
            case failure: InternalFailure => failure.copy(committed = true)
        case failure: InternalFailure => failure
    })

  def andLazyThen[A, B, C](parser1: P[A], parser2: => P[B])(using
    sequencer: Sequencer[A, B, C]
  ): AndLazyThen[A, C] = new AndLazyThen[A, C]:
    override def run(source: Source, offset: Int): InternalOut[(A, () => POut[C])] =
      parser1.run(source, offset) match
        case InternalSuccess(value1, start, end, label) =>
          InternalSuccess(
            (
              value1,
              () =>
                parser2.run(source, end) match
                  case InternalSuccess(value2, parsedStart, parsedEnd, nextLabel) =>
                    Success(
                      sequencer(value1, value2),
                      source.value.substring(parsedStart, parsedEnd),
                      source.value.substring(parsedEnd),
                      nextLabel
                    )
                  case InternalFailure(message, nextLabel, _) => Failure(message, nextLabel)
            ),
            start,
            end,
            label
          )
        case failure: InternalFailure => failure
    override def apply(input: String): POut[(A, () => POut[C])] =
      run(Source(input), 0) match
        case InternalSuccess(value, start, end, label) =>
          Success(value, input.substring(start, end), input.substring(end), label)
        case InternalFailure(message, label, _) => Failure(message, label)

  def mapAndLazyThen[A, B1, B2](
    andLazyThen: AndLazyThen[A, B1]
  )(f: B1 => B2): AndLazyThen[A, B2] = new AndLazyThen[A, B2]:
    override def run(source: Source, offset: Int): InternalOut[(A, () => POut[B2])] =
      andLazyThen.run(source, offset) match
        case InternalSuccess((value, next), start, end, label) =>
          InternalSuccess((value, () => next().map(f)), start, end, label)
        case failure: InternalFailure => failure
    override def apply(input: String): POut[(A, () => POut[B2])] =
      run(Source(input), 0) match
        case InternalSuccess(value, start, end, label) =>
          Success(value, input.substring(start, end), input.substring(end), label)
        case InternalFailure(message, label, _) => Failure(message, label)

  def evalAndLazyThen[A, B](andLazyThen: AndLazyThen[A, B]): P[B] = public(internal { (source, offset) =>
    andLazyThen.run(source, offset) match
      case InternalSuccess((_, next), start, _, _) =>
        next() match
          case Success(value, _, remaining, label) =>
            InternalSuccess(value, start, source.value.length - remaining.length, label)
          case Failure(message, label) => InternalFailure(message, label)
      case failure: InternalFailure => failure
  })

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
