# Audit findings

- [x] FIX: HIGH: Repetition accepts a successful zero-width parser and can recurse indefinitely, exhausting stack/resources (`src/main/scala/dev/vgerasimov/slowparse/parser.scala:302`) — fixed in 883c898
- [x] FIX: MEDIUM: `charsWhile` repeatedly concatenates strings, making long matching input quadratic in time/allocation (`src/main/scala/dev/vgerasimov/slowparse/parser.scala:174`) — fixed in 883c898
- [x] FIX: MEDIUM: Empty `anyFrom` and reversed `fromRange` selections throw from `reduce` during parser construction instead of failing predictably (`src/main/scala/dev/vgerasimov/slowparse/parser.scala:217`) — fixed in 883c898
- [x] FIX: HIGH: CI installs JDK 11 although Scala 3.8.3 needs JDK 17 or newer, so compilation cannot reliably run (`.github/workflows/scala.yml:14`) — fixed in 2e1c2d1
- [x] FIX: HIGH: The JMH benchmark subproject lacks its own Scala version setting because root project settings are not inherited by sibling projects (`build.sbt:42`) — fixed in 2e1c2d1
- [x] FIX: MEDIUM: Benchmark documentation promises throughput, latency, and allocation metrics although its default JMH command only reports throughput (`README.md:79`) — fixed in 2e1c2d1
- [ ] FIX: HIGH: Parser success tests ignore unconsumed input, masking prefix-acceptance bugs in complete-language parsers (`src/test/scala/dev/vgerasimov/slowparse/ParserTestSuite.scala:7`)
- [ ] FIX: HIGH: README JSON example uses illegal forward references and does not enforce end-of-input, contradicting its valid-JSON claim (`README.md:47`)
- [ ] FIX: MEDIUM: README installation version is inconsistent with the changelog and current build version (`README.md:10`)
