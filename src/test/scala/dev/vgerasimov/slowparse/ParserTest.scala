package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.POut.*
import org.scalacheck.{ Gen, Prop }
import org.scalacheck.Prop.*

class ParserTest extends munit.ScalaCheckSuite {

  test("*capture(anyChar)* should parse any single printable ASCII character") {
    forAll { (c: Char, tail: String) =>
      {
        val toParse = c.toString + tail
        val parser = anyChar.!
        parser(toParse) match
          case Success(value, _, remaining, _) =>
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
    val parser = ws
    assertEquals(parser(" "), Success((), " ", "", Some("ws")))
    assertEquals(parser("\n"), Success((), "\n", "", Some("ws")))
    assertEquals(parser("\t"), Success((), "\t", "", Some("ws")))
    assertEquals(parser("\r"), Success((), "\r", "", Some("ws")))
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
        val toParse = s"$c1$c2$tail"
        val parser = P(c1).! ~ P(c2).!
        parser(toParse) match
          case Success(value, _, remaining, _) =>
            assertEquals(value, s"$c1$c2")
            assertEquals(remaining, tail)
          case _: Failure => fail(s"not parsed: $toParse")
      }
    }
  }
}
