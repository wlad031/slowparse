[<img src="https://user-images.githubusercontent.com/3338307/227219594-609eb6d3-c982-447a-ba49-5d34ac86230b.png" width="120" align="left" />](icon)


# slowparse

[![build](https://img.shields.io/github/actions/workflow/status/wlad031/slowparse/scala.yml?label=CI&logo=GitHub&style=flat-square)](https://github.com/wlad031/slowparse/actions)

This is just my experiment with parser combinators and Scala 3. Highly inspired by [fastparse from Li Haoyi](https://com-lihaoyi.github.io/fastparse) and [talk from Scott Wlaschin](https://youtube.com/watch?v=RDalzi7mhdY). If you want to use parser combinators in your project, probably better use [fastparse](https://github.com/com-lihaoyi/fastparse).

## Install (sbt)

1. Install [GitHub packages plugin](https://github.com/djspiewak/sbt-github-packages).
2. Add `Resolver.githubPackages("wlad031")` resolver.
3. Include the latest documented release dependency:
```scala
libraryDependencies += "dev.vgerasimov" %% "slowparse" % "0.1.4",
```

`0.1.4` is the latest release recorded in this changelog. The build's default `0.2.1-SNAPSHOT` is a development version, not installation guidance.

## Example

The following parser will be able to parse a valid JSON. Also it can be found in [JsonParserTest.scala](https://github.com/wlad031/slowparse/blob/master/src/test/scala/dev/vgerasimov/slowparse/JsonParserTest.scala).

```scala
import dev.vgerasimov.slowparse.POut.*
import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.Parsers.given
import dev.vgerasimov.slowparse.Parsers.*

sealed trait Json
case object JNull extends Json
case class JBoolean(v: Boolean) extends Json
case class JNumber(v: Double) extends Json
case class JString(v: String) extends Json
case class JArray(v: List[Json]) extends Json
case class JObject(v: Map[String, Json]) extends Json

val pNull: P[JNull.type] = P("null").map(_ => JNull)
val pBool: P[JBoolean] = (P("true").! | P("false").!)
  .map(_.toBoolean)
  .map(JBoolean(_))
val pNum: P[JNumber] =
  (anyFrom("+-").? ~ d.+ ~ (P(".") ~ d.*).? ~ (anyFrom("Ee") ~ anyFrom("+-").? ~ d.*).?).!.map(_.toDouble)
    .map(JNumber(_))
val pStr: P[JString] =
  (P("\"") ~ until(!P("\\") ~ P("\""), (P("\\\"") | anyChar)).! ~ (!P("\\") ~ P("\"")))
    .map(JString(_))
lazy val pChoice: P[Json] = P(choice(pNull, pBool, pNum, pStr, pArr, pObj))
lazy val pArr: P[JArray] = P("[") ~~ pChoice
  .rep(sep = Some(ws0 ~ P(",") ~ ws0))
  .map(JArray(_)) ~~ P("]")
lazy val pObj: P[JObject] =
  val pair: P[(String, Json)] = pStr.map(_.v) ~~ P(":") ~~ pChoice
  val pairs: P[List[(String, Json)]] = pair.rep(sep = Some(ws0 ~ P(",") ~ ws0))
  P("{") ~~ pairs.map(_.toMap).map(JObject(_)) ~~ P("}")

// final parser: accepts optional surrounding whitespace and requires all input
val json: P[Json] = ws0 ~~ P(pObj | pArr) ~~ ws0 ~~ end
```

## Performance benchmarks

Manual JMH benchmarks exercise slowparse public combinator API against deterministic valid JSON fixtures:

| Scenario | Fixture | Approximate size |
| --- | --- | ---: |
| `parseSmall` | `benchmark/src/main/resources/fixtures/small.json` | 482 B |
| `parseMedium` | `benchmark/src/main/resources/fixtures/medium.json` | 9 KiB |
| `parseLarge` | `benchmark/src/main/resources/fixtures/large.json` | 148 KiB |

Compile benchmark code:

```bash
sbt --batch "benchmark/Jmh/compile"
```

Run short, repeatable measurements for each metric:

```bash
# Throughput operations per second
sbt --batch "benchmark/Jmh/run -bm thrpt -i 3 -wi 3 -f 1 -t 1 .*JsonParserBenchmark.*"

# Average per-operation time (latency)
sbt --batch "benchmark/Jmh/run -bm avgt -i 3 -wi 3 -f 1 -t 1 .*JsonParserBenchmark.*"

# Allocation rate and allocation per operation
sbt --batch "benchmark/Jmh/run -bm thrpt -prof gc -i 3 -wi 3 -f 1 -t 1 .*JsonParserBenchmark.*"
```

Results are advisory comparisons only: benchmarks do not run in CI and no performance threshold is enforced.
