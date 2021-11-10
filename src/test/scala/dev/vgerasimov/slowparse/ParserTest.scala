package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.Parser.*
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.*

class ParserTest extends munit.ScalaCheckSuite {

  test("*char* should parse any single printable ASCII character") {
    forAll { (c: Char) => {
        char(c)(c.toString) match {
          case ParsingResult.Success(value, remaining) =>
            assertEquals(value, c)
            assertEquals(remaining, "")
          case _: ParsingResult.Failure => fail(s"not parsed: $c")
        }
      }
    }
  }

  test("*andThen(char, char)* should parse any two single printable ASCII characters") {
    forAll { (c1: Char, c2: Char) => {
      val toParse = s"$c1$c2"
      andThen(char(c1), char(c2))(toParse) match {
        case ParsingResult.Success(value, remaining) =>
          assertEquals(value, (c1, c2))
          assertEquals(remaining, "")
        case _: ParsingResult.Failure => fail(s"not parsed: $toParse")
      }
    }
    }
  }
}
