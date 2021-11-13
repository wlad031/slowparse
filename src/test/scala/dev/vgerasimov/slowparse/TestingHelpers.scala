package dev.vgerasimov.slowparse

object TestingHelpers extends munit.ScalaCheckSuite:

  def testSuccess[A](parser: P[A])(toParse: String, expected: A) =
    parser(toParse) match
      case POut.Success(v, _, _, _) => assertEquals(v, expected)
      case x: POut.Failure          => fail(s"not parsed: $toParse, failure: $x")

  def testFailure[A](parser: P[A])(toParse: String) =
    parser(toParse) match
      case x: POut.Success[A] => fail(s"parsed: $toParse, success: $x")
      case x: POut.Failure    => ()
