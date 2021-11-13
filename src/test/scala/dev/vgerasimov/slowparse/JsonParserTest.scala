package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.POut.*
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.TestingHelpers.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class JsonParserTest extends munit.ScalaCheckSuite:

  sealed trait Json
  case object JNull extends Json
  case class JBoolean(v: Boolean) extends Json
  case class JNumber(v: Long | Float) extends Json
  case class JString(v: String) extends Json
  case class JArray(v: List[Json]) extends Json
  case class JObject(v: Map[String, Json]) extends Json

  val pNull: P[JNull.type] = P("null").map(_ => JNull).label("null")
  val pBool: P[JBoolean] =
    (P("true").map(_ => true) | P("false").map(_ => false)).map(JBoolean(_)).label("false or true")
  val pNum: P[JNumber] = (P("-").? ~ d.+.! ~ (P(".") ~ d.+.!).?).map {
    case (minus, v, None)     => JNumber(v.toLong * (if (minus.isDefined) -1 else 1))
    case (minus, v, Some(v1)) => JNumber(s"$v.$v1".toFloat * (if (minus.isDefined) -1 else 1))
  }.label("number")
  val pStr: P[JString] = (P("\"") ~ (!P("\"") ~ anyChar).rep().! ~ P("\"")).map(JString(_)).label("string")

  def pChoice: P[Json] = choice(pNull, pBool, pNum, pStr)

  def pArr: P[JArray] = P("[") ~ wss ~ (pChoice ~ P(",") ~ wss).*.map(JArray(_)) ~ P("]")
  def pObj: P[JObject] =
    P("{") ~ wss ~ (pStr.map(_.v) ~ wss ~ P(":") ~ wss ~ pChoice ~ wss ~ P(
      ","
    ) ~ wss).*.map(ls => JObject(ls.toMap)) ~ P("}")

  test("*pNull* should parse null") { testSuccess(pNull)("null", JNull) }
  test("*pBool* should parse false") { testSuccess(pBool)("false", JBoolean(false)) }
  test("*pBool* should parse true") { testSuccess(pBool)("true", JBoolean(true)) }

  test("*pNum* should parse an integer") {
    forAll { (n: Int) => testSuccess(pNum)(n.toString, JNumber(n.toLong)) }
  }

  test("*pNum* should parse a float") {
    forAll { (n: Float) => testSuccess(pNum)(n.toString, JNumber(n.toLong)) }
  }

  test("*pArr* should parse simple JSON array") {
    val toParse =
      """[
        |  "this is a string",
        |  10,
        |  true,
        |  123.456,
        |  null,
        |]""".stripMargin
    val expected = JArray(
      List(
        JString("this is a string"),
        JNumber(10L),
        JBoolean(true),
        JNumber(123.456f),
        JNull
      )
    )
    testSuccess(pArr)(toParse, expected)
  }

  test("*pObj* should parse simple JSON object") {
    val toParse =
      """{
        |  "string": "this is a string",
        |  "int": 10,
        |  "bool": true,
        |  "float": 123.456,
        |  "nullable": null,
        |}""".stripMargin
    val expected = JObject(
      Map(
        "string"   -> JString("this is a string"),
        "int"      -> JNumber(10L),
        "bool"     -> JBoolean(true),
        "float"    -> JNumber(123.456f),
        "nullable" -> JNull
      )
    )
    testSuccess(pObj)(toParse, expected)
  }
