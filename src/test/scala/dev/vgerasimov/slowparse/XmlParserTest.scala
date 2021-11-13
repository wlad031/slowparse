package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given
import dev.vgerasimov.slowparse.POut.*
import org.scalacheck.Prop.*
import org.scalacheck.Gen

class XmlParserTest extends munit.ScalaCheckSuite {

  test("flatmapped parser should parse an XML tag") {
    // val leftTag: P[String] = P("<") ~ (!P(">") ~ anyChar).rep().! ~ P(">")
    // def rightTag(s: String): P[String] = P("</") ~ P(s).! ~ P(">")
    // val xml =
    //   for {
    //     s     <- leftTag
    //     right <- rightTag(s)
    //   } yield right

    // forAllNoShrink(Gen.nonEmptyListOf(Gen.alphaNumChar)) { (ls: List[Char]) =>
    //   {
    //     val s = ls.map(_.toString).reduce(_ + _)
    //     val parser = xml
    //     val toParse = s"<$s></$s>"
    //     parser(toParse) match
    //       case Success(v, _, _, _) =>
    //         assertEquals(v, s)
    //       case x : Failure => fail(s"not parsed: $toParse\nwith failure = $x")
    //   }
    // }
  }
}
