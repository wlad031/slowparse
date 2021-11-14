package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.POut.*
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class XmlParserTest extends ParserTestSuite {

  test("flatmapped parser should parse an XML tag") {
    val leftTag: P[String] = P("<") ~ (!P(">") ~ anyChar).+.! ~ P(">")
    def rightTag(s: String): P[String] = P("</") ~ P(s).! ~ P(">")
    val xml =
      for {
        s     <- leftTag
        right <- rightTag(s)
      } yield right

    // FIXME: wtf is this Gen
    forAllNoShrink(Gen.nonEmptyListOf(Gen.alphaNumChar)) { (ls: List[Char]) =>
      {
        val s = ls.map(_.toString).reduce(_ + _)
        val parser = xml
        val toParse = s"<$s></$s>"
        parser(toParse) match
          case Success(v, _, _, _) =>
            assertEquals(v, s)
          case x: Failure => fail(s"not parsed: $toParse\nwith failure = $x")
      }
    }
  }
}
