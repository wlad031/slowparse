package dev.vgerasimov.slowparse

import scala.annotation.{ tailrec, targetName }
import scala.language.postfixOps

/** Function acception an input to be parsed and returning [[POut]]. */
trait P[+A] extends (String => POut[A]):
  /** Legacy-public parser adapter. Library parsers override this with span execution. */
  private[slowparse] def run(input: Span, offset: Int): PResult[A] =
    apply(input.materialize(offset)) match
      case POut.Success(value, parsed, remaining, label) =>
        PResult.Success(value, offset, input.size - Span.fromInput(remaining).size, label)
      case POut.Failure(message, label) => PResult.Failure(message, label)

  /** Default discard path. Parsers with an allocation-free discard implementation override it. */
  private[slowparse] def runDiscard(input: Span, offset: Int): PResult[Unit] =
    run(input, offset) match
      case PResult.Success(_, start, end, label) => PResult.Success((), start, end, label)
      case failure: PResult.Failure              => failure

/** A [[P]]arser that returns a pair of one value and a function that can be evaluated in order to "continue" parsing.
  */
trait AndLazyThen[A, +B] extends P[(A, () => POut[B])]

/** Represents a result of parsing. */
sealed trait POut[+A]

/** Contains implementations of [[POut]]. */
private[slowparse] final case class Span private (source: String, start: Int, end: Int):
  def size: Int = end
  def length: Int = end - start
  def at(offset: Int): Char = source.charAt(offset)
  def charAt(index: Int): Char = source.charAt(start + index)
  def advance(count: Int): Span = copy(start = start + count)
  def capture: String = source.substring(start, end)
  def containsAll(token: Span, offset: Int): Boolean =
    offset + token.length <= end && source.regionMatches(offset, token.source, token.start, token.length)
  def charSet: Set[Char] = source.substring(start, end).toSet

  /** Boundary helper. Only public results, semantic values, and captures materialize text. */
  def materialize(from: Int = 0, until: Int = Int.MaxValue): String =
    source.substring(from.max(start), until.min(end))

private[slowparse] object Span:
  /** Public String-input boundary. */
  def fromInput(input: String): Span = Span(input, 0, input.length)

/** Private offset result layer. */
private[slowparse] sealed trait PResult[+A]
private[slowparse] object PResult:
  final case class Success[+A](
    value: A,
    start: Int,
    end: Int,
    label: Option[String] = None
  ) extends PResult[A]

  final case class Failure(
    expected: String,
    label: Option[String] = None,
    committed: Boolean = false,
    at: Int = -1
  ) extends PResult[Nothing]

private[slowparse] trait InternalParser[+A]:
  def run(source: Span, offset: Int): PResult[A]

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
      val before = Span.fromInput(ctx._1)
      val after = Span.fromInput(ctx._2)
      val prefix = if before.size == 0 then "   " else "..."
      val suffix = if after.size == 0 then "   " else "..."
      val caretPadding = Vector.fill(before.size)(' ').mkString
      Failure(
        s"""|Expected: $expected
          |Got:      $got
          |   $prefix${before.materialize()}${after.materialize()}$suffix
          |      $caretPadding^
      """.stripMargin,
        parserLabel
      )

  def ctx(before: String = "", after: String = ""): (String, String) =
    val beforeSource = Span.fromInput(before)
    val afterSource = Span.fromInput(after)
    (beforeSource.materialize(beforeSource.size - 5), afterSource.materialize(0, 5))

/** Contains simple constructors for [[P]]. */
object P:

  /** Lazily calls given parser.
    *
    * Helpful for creating mutually recursive parsers.
    */
  def apply[A](parser: => P[A]): P[A] = new P[A]:
    override def run(source: Span, offset: Int): PResult[A] = parser.run(source, offset)
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

  private def render[A](parser: InternalParser[A], input: String): POut[A] =
    val source = Span.fromInput(input)
    parser.run(source, 0) match
      case PResult.Success(value, start, end, label) =>
        Success(value, source.materialize(start, end), source.materialize(end), label)
      case PResult.Failure(expected, label, _, at) => Failure(s"Expected: $expected\nOffset: ${at.max(0)}", label)

  private def public[A](parser: InternalParser[A]): P[A] = new P[A]:
    override def run(source: Span, offset: Int): PResult[A] = parser.run(source, offset)
    override def apply(input: String): POut[A] = render(parser, input)

  private def internal[A](f: (Span, Int) => PResult[A]): InternalParser[A] =
    new InternalParser[A]:
      override def run(source: Span, offset: Int): PResult[A] = f(source, offset)

  private def asInternal[A](parser: P[A]): InternalParser[A] = internal { (source, offset) =>
    parser(source.materialize(offset)) match
      case Success(value, parsed, remaining, label) =>
        PResult.Success(value, offset, source.size - Span.fromInput(remaining).size, label)
      case Failure(message, label) => PResult.Failure(message, label)
  }

  /** Always succeeding parser consuming no characters. */
  val success: P[Unit] = public(internal((_, offset) => PResult.Success((), offset, offset)))

  /** Always failing parser. */
  def fail[A]: P[A] = public(internal((_, offset) => PResult.Failure("this parser always fails", at = offset)))

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
    if offset == source.size then PResult.Success((), offset, offset)
    else PResult.Failure("<end of input>", at = offset)
  })

  /** Parses characters satisfying given condition. */
  def charsWhile(
    condition: Char => Boolean
  ): P[String] = public(internal { (source, offset) =>
    var end = offset
    while end < source.size && condition(source.at(end)) do end += 1
    PResult.Success(source.materialize(offset, end), offset, end)
  })

  /** Parses all characters until some of them is presented in given string. */
  def charsUntilIn(string: String): P[String] =
    val chars = Span.fromInput(string).charSet
    charsWhile(c => !chars.contains(c))

  /** Parses all characters until end of line. */
  val charsUntilEol: P[String] = charsUntilIn("\n\r")

  /** Positive-lookahead parser, consumes no input. */
  def & [A](parser: P[A]): P[A] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case PResult.Success(value, _, _, label) => PResult.Success(value, offset, offset, label)
      case failure: PResult.Failure            => failure
  })

  /** Parses returning failure only if input is empty. */
  val anyChar: P[Unit] = public(internal { (source, offset) =>
    if offset >= source.size then PResult.Failure("<any char>", at = offset)
    else PResult.Success((), offset, offset + 1)
  })

  /** Parses and returns one character without materializing an intermediate String. */
  val anyCharValue: P[Char] = public(internal { (source, offset) =>
    if offset >= source.size then PResult.Failure("<any char>", at = offset)
    else PResult.Success(source.at(offset), offset, offset + 1)
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
    val failure = PResult.Failure(char.toString)
    public(internal { (source, offset) =>
      if offset < source.size && source.at(offset) == char then PResult.Success((), offset, offset + 1)
      else failure
    })

  /** Parses given string */
  def string(str: String): P[Unit] =
    val token = Span.fromInput(str)
    val failure = PResult.Failure(str)
    public(internal { (source, offset) =>
      if source.containsAll(token, offset) then PResult.Success((), offset, offset + token.size)
      else failure
    })

  /** Attaches given label to the parser. */
  def label[A](parser: P[A])(string: String): P[A] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case PResult.Success(value, start, end, _)  => PResult.Success(value, start, end, Some(string))
      case PResult.Failure(expected, _, committed, at) => PResult.Failure(expected, Some(string), committed, at)
  })

  /** Applies given function to successful result of calling given parser. */
  def map[A, B](parser: P[A])(f: A => B): P[B] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case PResult.Success(value, start, end, label) => PResult.Success(f(value), start, end, label)
      case failure: PResult.Failure                  => failure
  })

  /** Applies given function returning another parser to successful result of calling given parser. */
  def flatMap[A, B](parser: P[A])(f: A => P[B]): P[B] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case PResult.Success(value, start, end, _) =>
        f(value).run(source, end) match
          case PResult.Success(nextValue, _, nextEnd, label) => PResult.Success(nextValue, start, nextEnd, label)
          case failure: PResult.Failure                      => failure
      case failure: PResult.Failure => failure
  })

  /** Wraps result of calling given parser into [[Option]], thus, never fails. */
  def optional[A](parser: P[A]): P[Option[A]] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case PResult.Success(value, start, end, label) => PResult.Success(Some(value), start, end, label)
      case _: PResult.Failure                        => PResult.Success(None, offset, offset)
  })

  /** Unwraps parser returning [[Option]] by failing if result is `None`. */
  def unOption[A](parser: P[Option[A]]): P[A] = map(filter(parser)(opt => opt != None))(_.get)

  /** Checks that parsed value satisfies given condition, if not - fails. */
  def filter[A](parser: P[A])(cond: A => Boolean): P[A] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case success @ PResult.Success(value, _, _, _) if cond(value) => success
      case PResult.Success(value, _, _, _) => PResult.Failure(s"parsed value $value doesn't satisfy given condition")
      case failure: PResult.Failure        => failure
  })

  /** Concatenates two given parsers. */
  def andThen[A, B, C](parser1: P[A], parser2: P[B])(using sequencer: Sequencer[A, B, C]): P[C] =
    public(internal { (source, offset) =>
      parser1.run(source, offset) match
        case PResult.Success(value1, start, end, _) =>
          parser2.run(source, end) match
            case PResult.Success(value2, _, next, _) => PResult.Success(sequencer(value1, value2), start, next)
            case failure: PResult.Failure => if failure.at < 0 then failure.copy(at = end) else failure
        case failure: PResult.Failure => if failure.at < 0 then failure.copy(at = offset) else failure
    })

  /** Concatenates two given parsers in a flatMap manner. */
  def andThenFlatMap[A, B, C](parser1: P[A], parser2: A => P[B])(using sequencer: Sequencer[A, B, C]): P[C] =
    public(internal { (source, offset) =>
      parser1.run(source, offset) match
        case PResult.Success(value1, start, end, _) =>
          parser2(value1).run(source, end) match
            case PResult.Success(value2, _, next, _) => PResult.Success(sequencer(value1, value2), start, next)
            case failure: PResult.Failure => if failure.at < 0 then failure.copy(at = end) else failure
        case failure: PResult.Failure => if failure.at < 0 then failure.copy(at = offset) else failure
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
    val collected = internal { (source, initialOffset) =>
      @tailrec def iter(i: Int, current: P[A], values: List[A], offset: Int): PResult[List[A]] =
        if i == max || (i == min && !greedy) then PResult.Success(values.reverse, initialOffset, offset)
        else if offset == source.size then
          if i < min then PResult.Failure(s"expected minimum $min repetions, but parsed only $i", at = offset)
          else PResult.Success(values.reverse, initialOffset, offset)
        else current.run(source, offset) match
          case PResult.Success(value, _, next, _) if next == offset && condition(value) =>
            PResult.Failure("repetition parser consumed no input", at = offset)
          case PResult.Success(value, _, next, _) if condition(value) =>
            iter(i + 1, nextParser, value :: values, next)
          case _: PResult.Success[?] if min <= i && i <= max => PResult.Success(values.reverse, initialOffset, offset)
          case _: PResult.Failure if min <= i && i <= max    => PResult.Success(values.reverse, initialOffset, offset)
          case _ => PResult.Failure("rep failed", at = offset)
      iter(0, parser, Nil, initialOffset)
    }
    val discarded = internal { (source, initialOffset) =>
      @tailrec def iter(i: Int, current: P[A], offset: Int): PResult[Unit] =
        if i == max || (i == min && !greedy) then PResult.Success((), initialOffset, offset)
        else if offset == source.size then
          if i < min then PResult.Failure(s"expected minimum $min repetions, but parsed only $i", at = offset)
          else PResult.Success((), initialOffset, offset)
        else current.run(source, offset) match
          case PResult.Success(value, _, next, _) if next == offset && condition(value) =>
            PResult.Failure("repetition parser consumed no input", at = offset)
          case PResult.Success(value, _, next, _) if condition(value) => iter(i + 1, nextParser, next)
          case _: PResult.Success[?] if min <= i && i <= max => PResult.Success((), initialOffset, offset)
          case _: PResult.Failure if min <= i && i <= max    => PResult.Success((), initialOffset, offset)
          case _ => PResult.Failure("rep failed", at = offset)
      iter(0, parser, initialOffset)
    }
    new P[List[A]]:
      override def run(source: Span, offset: Int): PResult[List[A]] = collected.run(source, offset)
      override def runDiscard(source: Span, offset: Int): PResult[Unit] = discarded.run(source, offset)
      override def apply(input: String): POut[List[A]] = render(collected, input)

  def capture(parser: P[?]): P[String] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case PResult.Success(_, start, end, label) =>
        PResult.Success(source.materialize(start, end), start, end, label)
      case failure: PResult.Failure => failure
  })

  /** Drops parsed value through parser-specific discard path; avoids repetition list materialization. */
  def unCapture(parser: P[?]): P[Unit] = public(internal { (source, offset) =>
    parser.runDiscard(source, offset)
  })

  def not(parser: P[?]): P[Unit] = public(internal { (source, offset) =>
    parser.run(source, offset) match
      case PResult.Success(value, _, _, _) => PResult.Failure(s"unexpected: $value", at = offset)
      case _: PResult.Failure              => PResult.Success((), offset, offset)
  })

  def orElse[A, B](parser1: P[A], parser2: P[B]): P[A | B] = public(internal { (source, offset) =>
    parser1.run(source, offset) match
      case success: PResult.Success[A]                   => success
      case failure: PResult.Failure if failure.committed => failure
      case _: PResult.Failure =>
        parser2.run(source, offset) match
          case success: PResult.Success[B] => success
          case failure: PResult.Failure    => if failure.at < 0 then failure.copy(at = offset) else failure
  })

  def fromRange(range: scala.collection.immutable.NumericRange.Inclusive[Char]): P[Unit] = choice(range.map(char)*)

  def fromRange(range: String): P[Unit] = charRange.+(range) match
    case Success(ranges, _, _, _) =>
      choice(ranges.map { case (fromChar, toChar) => fromRange(fromChar to toChar) }*)
    case _: Failure => ignoredInput => Failure(s"cannot parse given range: $range")
  private val charRange: P[(Char, Char)] =
    anyCharValue ~ P("-") ~ anyCharValue

  def cut[A, B, C](parser1: P[A], parser2: P[B])(using sequencer: Sequencer[A, B, C]): P[C] =
    public(internal { (source, offset) =>
      parser1.run(source, offset) match
        case PResult.Success(value1, start, end, _) =>
          parser2.run(source, end) match
            case PResult.Success(value2, _, next, label) =>
              PResult.Success(sequencer(value1, value2), start, next, label)
            case failure: PResult.Failure => failure.copy(committed = true)
        case failure: PResult.Failure => failure
    })

  def andLazyThen[A, B, C](parser1: P[A], parser2: => P[B])(using
    sequencer: Sequencer[A, B, C]
  ): AndLazyThen[A, C] = new AndLazyThen[A, C]:
    override def run(source: Span, offset: Int): PResult[(A, () => POut[C])] =
      parser1.run(source, offset) match
        case PResult.Success(value1, start, end, label) =>
          PResult.Success(
            (
              value1,
              () =>
                parser2.run(source, end) match
                  case PResult.Success(value2, parsedStart, parsedEnd, nextLabel) =>
                    Success(
                      sequencer(value1, value2),
                      source.materialize(parsedStart, parsedEnd),
                      source.materialize(parsedEnd),
                      nextLabel
                    )
                  case PResult.Failure(expected, nextLabel, _, at) => Failure(s"Expected: $expected\nOffset: ${at.max(0)}", nextLabel)
            ),
            start,
            end,
            label
          )
        case failure: PResult.Failure => failure
    override def apply(input: String): POut[(A, () => POut[C])] =
      val source = Span.fromInput(input)
      run(source, 0) match
        case PResult.Success(value, start, end, label) =>
          Success(value, source.materialize(start, end), source.materialize(end), label)
        case PResult.Failure(expected, label, _, at) => Failure(s"Expected: $expected\nOffset: ${at.max(0)}", label)

  def mapAndLazyThen[A, B1, B2](
    andLazyThen: AndLazyThen[A, B1]
  )(f: B1 => B2): AndLazyThen[A, B2] = new AndLazyThen[A, B2]:
    override def run(source: Span, offset: Int): PResult[(A, () => POut[B2])] =
      andLazyThen.run(source, offset) match
        case PResult.Success((value, next), start, end, label) =>
          PResult.Success((value, () => next().map(f)), start, end, label)
        case failure: PResult.Failure => failure
    override def apply(input: String): POut[(A, () => POut[B2])] =
      val source = Span.fromInput(input)
      run(source, 0) match
        case PResult.Success(value, start, end, label) =>
          Success(value, source.materialize(start, end), source.materialize(end), label)
        case PResult.Failure(expected, label, _, at) => Failure(s"Expected: $expected\nOffset: ${at.max(0)}", label)

  def evalAndLazyThen[A, B](andLazyThen: AndLazyThen[A, B]): P[B] = public(internal { (source, offset) =>
    andLazyThen.run(source, offset) match
      case PResult.Success((_, next), start, _, _) =>
        next() match
          case Success(value, _, remaining, label) =>
            PResult.Success(value, start, source.size - Span.fromInput(remaining).size, label)
          case Failure(message, label) => PResult.Failure(message, label)
      case failure: PResult.Failure => failure
  })

end Parsers

extension (self: P[List[?]]) def mkString: P[String] = self.map(_.mkString)

extension [A](self: POut[A])

  /** Applies given function to the value inside [[POut.Success]]. */
  private[slowparse] def map[B](f: A => B): POut[B] = self match
    case POut.Success(v, parsed, remaining, label) => POut.Success(f(v), parsed, remaining, label)
    case x: POut.Failure                           => x

  /** Drops parser's label from [[POut]]. */
  private[slowparse] def dropLabel: POut[A] = self match
    case POut.Success(v, parsed, remaining, _) => POut.Success(v, parsed, remaining, None)
    case POut.Failure(message, _)              => POut.Failure(message, None)
