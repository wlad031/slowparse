val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.1.0",
    organization := "dev.vgerasimov",
    name := "slowparse",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-rewrite", "-source", "future"
    ),
    libraryDependencies ++= {
      val munitVersion = "0.7.29"
      Seq(
        "org.scalameta" %% "munit"            % munitVersion % Test,
        "org.scalameta" %% "munit-scalacheck" % munitVersion % Test
      )
    }
)
