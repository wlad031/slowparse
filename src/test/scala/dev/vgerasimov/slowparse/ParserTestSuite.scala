package dev.vgerasimov.slowparse

/** Contains useful utilities for testing  parsers */
trait ParserTestSuite extends munit.ScalaCheckSuite:

  /** Checks that parser successfully parsed given string with expected result. */
  def testSuccess[A](parser: P[A])(toParse: String, expected: A) = parser(toParse) match
    case POut.Success(v, p, r, l) =>
      assertEquals(
        v,
        expected,
        s"""Wrong "Success" result:
           |  parsed:    $p
           |  remaining: $r
           |""".stripMargin
      )
    case POut.Failure(msg, _) => fail(s"not parsed: $toParse, failure: \n$msg")

  /** Checks that parser couldn't parse given string. */
  def testFailure[A](parser: P[A])(toParse: String) = parser(toParse) match
    case x: POut.Success[A] => fail(s"parsed: $toParse, success: $x")
    case x: POut.Failure    => ()
