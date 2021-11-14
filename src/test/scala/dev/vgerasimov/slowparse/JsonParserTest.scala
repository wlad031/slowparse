package dev.vgerasimov.slowparse

import dev.vgerasimov.slowparse.POut.*
import dev.vgerasimov.slowparse.Parsers.given
import dev.vgerasimov.slowparse.Parsers.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class JsonParserTest extends ParserTestSuite:

  sealed trait Json
  case object JNull extends Json
  case class JBoolean(v: Boolean) extends Json
  // TODO: make it easily accept integers
  case class JNumber(v: Long | Float) extends Json
  case class JString(v: String) extends Json
  case class JArray(v: List[Json]) extends Json
  case class JObject(v: Map[String, Json]) extends Json

  val pNull: P[JNull.type] = P("null").map(_ => JNull)
  val pBool: P[JBoolean] = (P("true").! | P("false").!).map(_.toBoolean).map(JBoolean(_))

  // TODO: refactor this
  val pNum: P[JNumber] = (P("-").? ~ d.+.! ~ (P(".") ~ d.+.!).?).map {
    case (minus, v, None)     => JNumber(v.toLong * (if (minus.isDefined) -1 else 1))
    case (minus, v, Some(v1)) => JNumber(s"$v.$v1".toFloat * (if (minus.isDefined) -1 else 1))
  }

  val pStr: P[JString] = (P("\"") ~ (!P("\"") ~ anyChar).rep().! ~ P("\"")).map(JString(_))
  val pChoice: P[Json] = P(choice(pNull, pBool, pNum, pStr, pArr, pObj))
  val pArr: P[JArray] = P("[") ~~ pChoice.rep(sep = (wss ~ P(",") ~ wss)).map(JArray(_)) ~~ P("]")
  val pObj: P[JObject] =
    val pair: P[(String, Json)] = pStr.map(_.v) ~~ P(":") ~~ pChoice
    val pairs: P[List[(String, Json)]] = pair.rep(sep = (wss ~ P(",") ~ wss))
    P("{") ~~ pairs.map(_.toMap).map(JObject(_)) ~~ P("}")

  val json: P[Json] = P(pObj | pArr)

  test("*pNull* should parse null") { testSuccess(pNull)("null", JNull) }
  test("*pBool* should parse false") { testSuccess(pBool)("false", JBoolean(false)) }
  test("*pBool* should parse true") { testSuccess(pBool)("true", JBoolean(true)) }

  test("*pNum* should parse an integer") {
    forAll { (n: Int) => testSuccess(pNum)(n.toString, JNumber(n.toLong)) }
  }

  test("*pArr* should parse simple JSON array") {
    val toParse =
      """[
        |  "this is a string",
        |  10,
        |  true,
        |  123.456,
        |  null
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
        |  "nullable": null
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

  test("*json* should parse more complex JSON object") {
    val toParse =
      """{
        |  "string": "this is a string",
        |  "emptyList":[],
        |  "o1":{"innerBool":false,"arr": [1, 2, 3] },
        |  "a1":     [{}, null, {"foo": null}]
        |}""".stripMargin
    val expected = JObject(
      Map(
        "string" -> JString("this is a string"),
        "emptyList" -> JArray(Nil),
        "o1" -> JObject(
          Map("innerBool" -> JBoolean(false), "arr" -> JArray(List(JNumber(1L), JNumber(2L), JNumber(3L))))
        ),
        "a1" -> JArray(List(JObject(Map()), JNull, JObject(Map("foo" -> JNull))))
      )
    )
    testSuccess(json)(toParse, expected)
  }
