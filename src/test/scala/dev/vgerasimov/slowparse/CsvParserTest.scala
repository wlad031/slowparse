package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.POut.*
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class CsvParserTest extends ParserTestSuite {
  
  val parser = (charsUntilIn(",\n") ~ (P(",") ~ charsUntilIn(",\n")).rep() ~ P(",").?.!!)
      .map({ case (first: String, rest: List[String]) => first :: rest })

  test("CSV parser should parse one line of CSV values") {
    val toParse = "hello,world,!"
    testSuccess(parser)(toParse, List("hello", "world", "!"))
  }
}
