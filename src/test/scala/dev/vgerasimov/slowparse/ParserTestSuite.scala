package dev.vgerasimov.slowparse

/** Contains useful utilities for testing  parsers */
trait ParserTestSuite extends munit.ScalaCheckSuite:

  /** Checks that parser successfully parsed given string with expected result. */
  def testSuccess[A](parser: P[A])(toParse: String, expected: A) = parser(toParse) match
    case POut.Success(v, _, _, _) => assertEquals(v, expected)
    case x: POut.Failure          => fail(s"not parsed: $toParse, failure: $x")

  /** Checks that parser couldn't parse given string. */
  def testFailure[A](parser: P[A])(toParse: String) = parser(toParse) match
    case x: POut.Success[A] => fail(s"parsed: $toParse, success: $x")
    case x: POut.Failure    => ()
