package dev.vgerasimov.slowparse

import munit.FunSuite
import Parsers.given

class OffsetDiagnosticsTest extends FunSuite:
  private def failureMessage[A](parser: P[A], input: String): String =
    parser(input) match
      case POut.Failure(message, _) => message
      case success                  => fail(s"expected failure, got $success")

  test("primitive failures report expected token and absolute offset") {
    assertEquals(failureMessage(P('a'), "b"), "Expected: a\nOffset: 0")
    assertEquals(failureMessage(P("abc"), "abd"), "Expected: abc\nOffset: 0")
    assertEquals(failureMessage(Parsers.anyChar, ""), "Expected: <any char>\nOffset: 0")
  }

  test("sequencing, choice, and lookahead preserve failing absolute offset") {
    assertEquals(failureMessage(P('a') ~ P('b'), "ac"), "Expected: b\nOffset: 1")
    assertEquals(failureMessage(P('a') | P('b'), "c"), "Expected: b\nOffset: 0")
    assertEquals(failureMessage(Parsers.&(P('a')), "b"), "Expected: a\nOffset: 0")
  }

  test("repetition and negative lookahead expose final failure offset") {
    assertEquals(failureMessage(P('a').rep(min = 2), "a"), "Expected: expected minimum 2 repetions, but parsed only 1\nOffset: 1")
    assertEquals(failureMessage(!P('a'), "a"), "Expected: unexpected: ()\nOffset: 0")
  }

  test("capture materializes only captured span and discard capture keeps public span") {
    val parser = P('a').! ~ P('b').!!
    parser("abc") match
      case POut.Success(value, parsed, remaining, _) =>
        assertEquals(value, "a")
        assertEquals(parsed, "ab")
        assertEquals(remaining, "c")
      case failure => fail(s"expected success, got $failure")
  }
