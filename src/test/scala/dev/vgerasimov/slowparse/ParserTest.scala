package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given
import dev.vgerasimov.slowparse.POut.*
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import scala.annotation.alpha

class ParserTest extends ParserTestSuite:

  test("*end* should parse empty line") { testSuccess(end)("", ()) }

  test("*capture(anyChar)* should parse any single printable ASCII character") {
    forAll { (c: Char, tail: String) =>
      {
        val toParse = c.toString + tail
        val parser = anyChar.!
        parser(toParse) match
          case Success(value, _, remaining, _) =>
            assertEquals(value, c.toString)
            assertEquals(value, c.toString)
            assertEquals(remaining, tail)
          case _: Failure => fail(s"not parsed: $toParse")
      }
    }
  }

  test("*capture(char)* should parse a single printable ASCII character") {
    forAll { (c: Char, tail: String) =>
      {
        val toParse = c.toString + tail
        val parser = P(c).!
        parser(toParse) match
          case Success(value, _, remaining, _) =>
            assertEquals(value, c.toString)
            assertEquals(remaining, tail)
          case _: Failure => fail(s"not parsed: $toParse")
      }
    }
  }

  test("*not(char)* should not parse same single printable ASCII character") {
    forAll { (c: Char, tail: String) =>
      {
        val toParse = c.toString + tail
        val parser = !P(c).!
        parser(toParse) match
          case Success((), "", parsed, _) =>
            assertEquals(parsed, toParse)
          case x: Success[Unit] => fail(s"parsed not correctly: $toParse")
          case Failure(message, _) =>
            assertEquals(message, s"unexpected: $c")
      }
    }
  }

  test("*not(char)* should parse another character") {
    val toParse = "b"
    val parser = !P('a')
    parser(toParse) match
      case Success((), "", parsed, _) =>
        assertEquals(parsed, toParse)
      case x: Success[Unit] => fail(s"parsed not correctly: $toParse")
      case _: Failure       => fail(s"not parsed: $toParse")
  }

  test("*ws* should parse any whitespace character") {
    testSuccess(ws)(" ", ())
    testSuccess(ws)("\n", ())
    testSuccess(ws)("\t", ())
    testSuccess(ws)("\r", ())
  }

  test("*capture(string)* should parse a string") {
    forAll { (s: String, tail: String) =>
      {
        val toParse = s + tail
        val parser = P(s).!
        parser(toParse) match
          case Success(value, _, remaining, _) =>
            assertEquals(value, s)
            assertEquals(remaining, tail)
          case _: Failure => fail(s"not parsed: $toParse")
      }
    }
  }

  test("*optional(capture(char))* should parse empty string") {
    forAll { (c: Char) =>
      {
        val toParse = ""
        val parser = P(c).!.?
        parser(toParse) match
          case Success(None, _, remaining, _) =>
            assertEquals(remaining, toParse)
          case Success(Some(_), _, _, _) => fail(s"parsed: $c")
          case _: Failure                => fail(s"not parsed: $c")
      }
    }
  }

  test("*optional(capture(char))* should parse a single printable ASCII character") {
    forAll { (c: Char) =>
      {
        val toParse = c.toString
        val parser = P(c).!.?
        parser(toParse) match
          case Success(Some(value), _, remaining, _) =>
            assertEquals(value, toParse)
            assertEquals(remaining, "")
          case Success(None, _, _, _) => fail(s"not parsed: $c")
          case _: Failure             => fail(s"not parsed: $c")
      }
    }
  }

  test("*andThen(capture(char), capture(char))* should parse two single printable ASCII characters") {
    forAll { (c1: Char, c2: Char, tail: String) =>
      {
        val parser = (P(c1).! ~ P(c2).!).map { case (a, b) => s"$a$b" }
        val toParse = s"$c1$c2$tail"
        parser(toParse) match
          case Success(value, _, remaining, _) =>
            assertEquals(value, s"$c1$c2")
            assertEquals(remaining, tail)
          case _: Failure => fail(s"not parsed: $toParse")
      }
    }
  }

  test("*capture(d.rep(1, max, greedy))* should parse any number of digits") {
    val parser = d.rep().!
    forAllNoShrink(Gen.numStr) { (s: String) =>
      {
        val toParse = s
        parser(toParse) match
          case Success(v, _, remaining, _) =>
            assertEquals(v, s)
            assertEquals(remaining, "")
          case _: Failure => fail(s"not parsed: $toParse")
      }
    }
  }

  test("""*"a".*.!* should parse any number character "a" including 0""") {
    val parser = P("a").*.!
    testSuccess(parser)("a", "a")
    testSuccess(parser)("aaa", "aaa")
    testSuccess(parser)("", "")
  }

  test("""*"a".+.!* should parse any number character "a" excluding 0""") {
    val parser = P("a").+.!
    testSuccess(parser)("a", "a")
    testSuccess(parser)("aaa", "aaa")
    testFailure(parser)("")
  }

  test("*fromRange* should parse given ranges") {
    forAllNoShrink(Gen.alphaLowerChar) { (x: Char) => testSuccess(fromRange("a-z").!)(x.toString, x.toString) }
    forAllNoShrink(Gen.alphaUpperChar) { (x: Char) => testSuccess(fromRange("A-Z").!)(x.toString, x.toString) }
    forAllNoShrink(Gen.alphaChar) { (x: Char) => testSuccess(fromRange("a-zA-Z").!)(x.toString, x.toString) }
    forAllNoShrink(Gen.alphaNumChar) { (x: Char) => testSuccess(fromRange("a-z0-9A-Z").!)(x.toString, x.toString) }
  }

  test("non-greedy *rep(min, max)* should work properly") {
    val parser = P("a").rep(min = 3, max = 5, greedy = false).!
    testSuccess(parser)("aaa", "aaa")
    testSuccess(parser)("aaaa", "aaa")
    testSuccess(parser)("aaaaaa", "aaa")
    testSuccess(parser)("aaaaaaa", "aaa")
    testFailure(parser)("a")
    testFailure(parser)("aa")
  }

  test("greedy *rep(min, max)* should work properly") {
    val parser = P("a").rep(min = 3, max = 5, greedy = true).!
    testSuccess(parser)("aaa", "aaa")
    testSuccess(parser)("aaaa", "aaaa")
    testSuccess(parser)("aaaaaa", "aaaaa")
    testSuccess(parser)("aaaaaaa", "aaaaa")
    testFailure(parser)("a")
    testFailure(parser)("aa")
  }

  test("*until* should parse all characters before given one") {
    val parser = until(P("{")).!
    testSuccess(parser)("", "")
    testSuccess(parser)("hello world", "hello world")
    testSuccess(parser)("hello world{", "hello world")
    testSuccess(parser)("hello{} world", "hello")
    testSuccess(parser)("{hello world", "")
  }

  test("*code blocks* should parse divide given string into code blocks") {
    val parser = P(P("{") ~ until(P("}")).! ~ P("}")).*
    testSuccess(parser)("", Nil)
    testSuccess(parser)("{hello world}", List("hello world"))
    testSuccess(parser)("{hello world 1}{hello world 2}", List("hello world 1", "hello world 2"))
  }

  test("*blocks* should parse divide given string into blocks") {
    val parser = P((P("{") ~ until(P("}")).! ~ P("}")) | until(P("{")).!).*
    testSuccess(parser)("", Nil)
    testSuccess(parser)("hello world", List("hello world"))
    testSuccess(parser)("hello{} world", List("hello", "", " world"))
    testSuccess(parser)("{hello world}", List("hello world"))
  }

  test("*charsWhile* should parse all leading characters satisfying condition") {
    val parser = charsWhile(_.isLower)
    testSuccess(parser)("aA", "a")
    testSuccess(parser)("abcAbc", "abc")
  }

  test("*charsUntilIn* should parse all characters which are not in the given string") {
    val parser = charsUntilIn("123")
    testSuccess(parser)("abc", "abc")
    testSuccess(parser)("ab1c", "ab")
    testSuccess(parser)("ab2c", "ab")
    testSuccess(parser)("ab3c", "ab")
  }

  test("*charsUntilEol* should parse all characters which are not end of line") {
    val parser = charsUntilEol
    testSuccess(parser)("abc", "abc")
    testSuccess(parser)("abc\n", "abc")
    testSuccess(parser)("ab\nc", "ab")
    testSuccess(parser)("\nabc", "")
  }

  test("*surrounded* should parse everything surrounded by two parsers") {
    val parser = surrounded(P("a"), P("b"), d.+.!)
    testSuccess(parser)("a123b", "123")
    testSuccess(parser)("a12b3b", "12")
    testFailure(parser)("a*1*b")
  }

  test("*surrounded* should parse everything surrounded by one parser") {
    val parser = surrounded(P("*"), d.+.!)
    testSuccess(parser)("*123*", "123")
    testSuccess(parser)("*12*", "12")
    testFailure(parser)("*12abc*")
  }

  test("*surrounded* should parse all characters surrounded by one parser") {
    val parser = surrounded(P("```"))
    testSuccess(parser)("```\nhello\nworld```", "\nhello\nworld")
    testFailure(parser)("``fail``")
  }

  test("*cut* fails when needed") {
    val andThenParser: P[String] = P(P("val ") ~ alpha.rep(1).! | P("def ") ~ alpha.rep(1).!)
    testFailure(andThenParser)("val 1234")
    val cutParser: P[String] = P(P("val ") ~/ alpha.rep(1).! | P("def ") ~/ alpha.rep(1).!)
    testFailure(cutParser)("val 1234")
  }

  test("*andThenDelayed* + *mapContinuable* is working properly") {
    val parser1: ContinuableP[Int, (Int, String)] = andThenDelayed(d.!.map(_.toInt), P("A").!)
    val parser2: ContinuableP[Int, String] = mapContinuable(parser1) { case (x: Int, s: String) =>
      s.repeat(x)
    }

    parser1("3A") match
      case s @ Success((value1, f), parsed1, remaining1, _) =>
        assertEquals(value1, 3)
        assertEquals(parsed1, "3")
        assertEquals(remaining1, "A")
        f() match
          case Success(value2, parsed2, remaining3, _) =>
            assertEquals(value2, (3, "A"))
            assertEquals(parsed2, "A")
            assertEquals(remaining3, "")
          case f: Failure =>
            fail(s"not continued: $f")
      case Failure(message, _) =>
        fail(s"not parsed: $message")

    parser2("3A") match
      case s @ Success((value1, f), parsed1, remaining1, _) =>
        assertEquals(value1, 3)
        assertEquals(parsed1, "3")
        assertEquals(remaining1, "A")
        f() match
          case Success(value2, parsed2, remaining3, _) =>
            assertEquals(value2, "AAA")
            assertEquals(parsed2, "A")
            assertEquals(remaining3, "")
          case f: Failure =>
            fail(s"not continued: $f")
      case Failure(message, _) =>
        fail(s"not parsed: $message")

  }
