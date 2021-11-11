package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.Parsers.*
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.*

class ParserTest extends munit.ScalaCheckSuite {

  test("*capture(char)* should parse a single printable ASCII character") {
    forAll { (c: Char, tail: String) => {
        val toParse = c.toString + tail
        val parser = char(c).!
        parser(toParse) match {
          case POut.Success(value, _, remaining, _) =>
            assertEquals(value, c.toString)
            assertEquals(remaining, tail)
          case _: POut.Failure => fail(s"not parsed: $toParse")
        }
      }
    }
  }

  test("*capture(string)* should parse a string") {
    forAll { (s: String, tail: String) => {
        val toParse = s + tail
        val parser = string(s).!
        parser(toParse) match {
          case POut.Success(value, _, remaining, _) =>
            assertEquals(value, s)
            assertEquals(remaining, tail)
          case _: POut.Failure => fail(s"not parsed: $toParse")
        }
      }
    }
  }

  test("*optional(capture(char))* should parse empty string") {
    forAll { (c: Char) => {
        val toParse = ""
        val parser = char(c).!.?
        parser(toParse) match {
          case POut.Success(None, _, remaining, _) =>
            assertEquals(remaining, toParse)
          case POut.Success(Some(_), _, _, _) => fail(s"parsed: $c")
          case _: POut.Failure => fail(s"not parsed: $c")
        }
      }
    }
  }

  test("*optional(capture(char))* should parse a single printable ASCII character") {
    forAll { (c: Char) => {
        val toParse = c.toString
        val parser = char(c).!.?
        parser(toParse) match {
          case POut.Success(Some(value), _, remaining, _) =>
            assertEquals(value, toParse)
            assertEquals(remaining, "")
          case POut.Success(None, _, _, _) => fail(s"not parsed: $c")
          case _: POut.Failure => fail(s"not parsed: $c")
        }
      }
    }
  }

  test("*andThen(capture(char), capture(char))* should parse two single printable ASCII characters") {
    forAll { (c1: Char, c2: Char, tail: String) => {
        val toParse = s"$c1$c2$tail"
        val parser = andThen(char(c1).!, char(c2).!)
        parser(toParse) match {
          case POut.Success(value, _, remaining, _) =>
            assertEquals(value, s"$c1$c2")
            assertEquals(remaining, tail)
          case _: POut.Failure => fail(s"not parsed: $toParse")
        }
      }
    }
  }
}
