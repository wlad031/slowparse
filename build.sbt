import Tasks.generateSequencers

val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.3.1",
    organization := "dev.vgerasimov",
    name := "slowparse",
    version := "0.2.1",
    githubOwner := "wlad031",
    githubRepository := "slowparse",
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    publishM2Configuration := publishM2Configuration.value.withOverwrite(true),
    scalacOptions ++= Seq(
      "-rewrite",
      "-source", "future"
    ),
    libraryDependencies ++= {
      val munitVersion = "0.7.29"
      Seq(
        "org.scalameta" %% "munit"            % munitVersion % Test,
        "org.scalameta" %% "munit-scalacheck" % munitVersion % Test
      )
    },
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "dev" / "vgerasimov" / "slowparse" / "Sequencers.scala"
      IO.write(file, generateSequencers(22))
      Seq(file)
    }.taskValue
  )
