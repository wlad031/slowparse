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
  case class JNumber(v: Double) extends Json
  case class JString(v: String) extends Json
  case class JArray(v: List[Json]) extends Json
  case class JObject(v: Map[String, Json]) extends Json

  val pNull: P[JNull.type] = P("null").map(_ => JNull)
  val pBool: P[JBoolean] = (P("true").! | P("false").!).map(_.toBoolean).map(JBoolean(_))
  val pNum: P[JNumber] =
    (anyFrom("+-").? ~ d.+ ~ (P(".") ~ d.*).? ~ (anyFrom("Ee") ~ anyFrom("+-").? ~ d.*).?).!.map(_.toDouble)
      .map(JNumber(_))
  val pStr: P[JString] =
    (P("\"") ~ until((!P("\\") ~ P("\"")), (P("\\\"") | anyChar)).! ~ (!P("\\") ~ P("\""))).map(JString(_))
  //noinspection ForwardReference
  val pChoice: P[Json] = P(choice(pNull, pBool, pNum, pStr, pArr, pObj))
  val pArr: P[JArray] = P("[") ~~ pChoice.rep(sep = Some(ws0 ~ P(",") ~ ws0)).map(JArray.apply) ~~ P("]")
  val pObj: P[JObject] =
    val pair: P[(String, Json)] = pStr.map(_.v) ~~ P(":") ~~ pChoice
    val pairs: P[List[(String, Json)]] = pair.rep(sep = Some(ws0 ~ P(",") ~ ws0))
    P("{") ~~ pairs.map(_.toMap).map(JObject.apply) ~~ P("}")

  val json: P[Json] = P(pObj | pArr)

  test("*pNull* should parse null") { testSuccess(pNull)("null", JNull) }
  test("*pBool* should parse false") { testSuccess(pBool)("false", JBoolean(false)) }
  test("*pBool* should parse true") { testSuccess(pBool)("true", JBoolean(true)) }

  test("*pStr* should parse quoted string") { testSuccess(pStr)(""""hello"""", JString("hello")) }

  test("*pStr* should parse a string containing escaped quotes") {
    testSuccess(pStr)(""""h\"el\"lo"""", JString("""h\"el\"lo"""))
  }

  test("*pNum* should parse an integer") {
    forAll { (n: Int) => testSuccess(pNum)(n.toString, JNumber(n.toDouble)) }
  }

  test("*pNum* should parse a float") {
    forAll { (n: Double) => testSuccess(pNum)(n.toString, JNumber(n.toDouble)) }
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
        JNumber(123.456d),
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
        "int"      -> JNumber(10),
        "bool"     -> JBoolean(true),
        "float"    -> JNumber(123.456d),
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
        "string"    -> JString("this is a string"),
        "emptyList" -> JArray(Nil),
        "o1" -> JObject(
          Map("innerBool" -> JBoolean(false), "arr" -> JArray(List(JNumber(1), JNumber(2), JNumber(3))))
        ),
        "a1" -> JArray(List(JObject(Map()), JNull, JObject(Map("foo" -> JNull))))
      )
    )
    testSuccess(json)(toParse, expected)
  }
