# JSON parser allocation baseline

Command:

```sh
java -jar /home/geras/.cache/assertj4s/sbt/sbt-launch-1.10.11.jar \
  'benchmark/Jmh/run -wi 1 -i 1 -f 1 -prof gc .*JsonParserBenchmark.(parseMediumInternal|parseLargeInternal)'
```

Environment: JDK 25.0.4.1.1 Graal, JMH 1.37, one fork, one 10-second warmup and measurement iteration.

| Scenario | Revision | Throughput | Allocation |
| --- | --- | ---: | ---: |
| medium internal | pre-span (`HEAD:parser.scala`) | 692.172 ops/s | 5,196,681.042 B/op |
| large internal | pre-span (`HEAD:parser.scala`) | 45.256 ops/s | 80,080,431.611 B/op |
| medium internal | current span + cached primitive failures | 892.443 ops/s | 4,461,424.792 B/op |
| large internal | current span + cached primitive failures | not captured | 76,276,021.318 B/op |
| medium public | current span + cached primitive failures | 778.874 ops/s | 5,014,000.908 B/op |
| large public | current span + cached primitive failures | 42.234 ops/s | 90,453,000.719 B/op |

Internal benchmarks exclude mandated `POut.Success.parsed` and `.remaining` materialization. Span input improves internal allocation by 14.1% on medium and 4.8% on large, still below required 50%. Do not treat this as a passing benchmark gate. Next implementation step: eliminate per-combinator result allocation through span-advancing result propagation, then rerun this exact command with stronger warmup/measurement settings.
