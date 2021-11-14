# slowparse

[![build](https://img.shields.io/github/workflow/status/wlad031/slowparse/Scala%20CI?label=CI&logo=GitHub&style=flat-square)](https://github.com/wlad031/slowparse/actions)

This is just my experiment with parser combinators and Scala 3. Highly inspired by [fastparse from Li Haoyi](https://com-lihaoyi.github.io/fastparse) and [talk from Scott Wlaschin](https://youtube.com/watch?v=RDalzi7mhdY). If you want to use parser combinators in your project, probably better use [fastparse](https://github.com/com-lihaoyi/fastparse).

## Example

The following parser will be able to parse a valid JSON. Also it can be found in [JsonParserTest.scala](https://github.com/wlad031/slowparse/blob/master/src/test/scala/dev/vgerasimov/slowparse/JsonParserTest.scala).

```scala
import dev.vgerasimov.slowparse.Parsers.given
import dev.vgerasimov.slowparse.Parsers.*

// ... definitions of JSON classes are pretty straighforward, thus, omitted ...

val pNull: P[JNull.type] = P("null").map(_ => JNull)
val pBool: P[JBoolean] = (P("true").! | P("false").!).map(_.toBoolean).map(JBoolean(_))
val pNum: P[JNumber] =
  (anyCharIn("+-").? ~ d.+ ~ (P(".") ~ d.*).? ~ (anyCharIn("Ee") ~ anyCharIn("+-").? ~ d.*).?).!.map(_.toDouble)
    .map(JNumber(_))
val pStr: P[JString] = (P("\"") ~ until(P("\"")).! ~ P("\"")).map(JString(_))
val pChoice: P[Json] = P(choice(pNull, pBool, pNum, pStr, pArr, pObj))
val pArr: P[JArray] = P("[") ~~ pChoice.rep(sep = wss ~ P(",") ~ wss).map(JArray(_)) ~~ P("]")
val pObj: P[JObject] =
  val pair: P[(String, Json)] = pStr.map(_.v) ~~ P(":") ~~ pChoice
  val pairs: P[List[(String, Json)]] = pair.rep(sep = wss ~ P(",") ~ wss)
  P("{") ~~ pairs.map(_.toMap).map(JObject(_)) ~~ P("}")

val json: P[Json] = P(pObj | pArr)
```
