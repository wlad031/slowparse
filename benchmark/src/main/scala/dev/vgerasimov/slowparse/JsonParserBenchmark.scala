package dev.vgerasimov.slowparse

import java.nio.charset.StandardCharsets

import org.openjdk.jmh.annotations.{ Benchmark, Level, Scope, Setup, State }

import dev.vgerasimov.slowparse.POut.*
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given

/** JMH scenarios for public slowparse combinator API on deterministic JSON fixtures. */
@State(Scope.Benchmark)
class JsonParserBenchmark:
  private var small = ""
  private var medium = ""
  private var large = ""

  @Setup(Level.Trial)
  def loadFixtures(): Unit =
    small = JsonFixtures.load("small")
    medium = JsonFixtures.load("medium")
    large = JsonFixtures.load("large")
    JsonFixtures.validate(small)
    JsonFixtures.validate(medium)
    JsonFixtures.validate(large)

  @Benchmark
  def parseSmall(): POut[Unit] = JsonFixtures.parser(small)

  @Benchmark
  def parseMedium(): POut[Unit] = JsonFixtures.parser(medium)

  @Benchmark
  def parseLarge(): POut[Unit] = JsonFixtures.parser(large)

private object JsonFixtures:
  private val jsonNull: P[Unit] = P("null")
  private val jsonBoolean: P[Unit] = P("true") | P("false")
  private val jsonNumber: P[Unit] =
    (anyFrom("+-").? ~ d.+ ~ (P(".") ~ d.*).? ~ (anyFrom("Ee") ~ anyFrom("+-").? ~ d.*).?).!!
  private val jsonString: P[Unit] =
    P("\"") ~~ until((!P("\\") ~ P("\"")), P("\\\"") | anyChar) ~~ (!P("\\") ~ P("\""))
  private val jsonValue: P[Unit] = P(choice(jsonNull, jsonBoolean, jsonNumber, jsonString, jsonArray, jsonObject))
  private val jsonArray: P[Unit] =
    P("[") ~~ jsonValue.rep(sep = Some(ws0 ~ P(",") ~ ws0)).!! ~~ P("]")
  private val jsonObject: P[Unit] =
    val pair = jsonString ~~ P(":") ~~ jsonValue
    P("{") ~~ pair.rep(sep = Some(ws0 ~ P(",") ~ ws0)).!! ~~ P("}")

  val parser: P[Unit] = P(jsonObject | jsonArray) ~~ end

  def load(name: String): String =
    val stream = Option(getClass.getResourceAsStream(s"/fixtures/$name.json"))
      .getOrElse(throw IllegalStateException(s"Missing benchmark fixture: $name.json"))
    try String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()

  def validate(input: String): Unit =
    parser(input) match
      case _: Success[?] => ()
      case failure: Failure => throw IllegalStateException(s"Invalid benchmark fixture: ${failure.message}")
