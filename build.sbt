val giteaMaven = "https://gitea.local.vgerasimov.dev/api/packages/wlad031/maven"
val artifactVersion = sys.env.getOrElse("VERSION", "0.2.1-SNAPSHOT")
val giteaCredentials = for {
  username <- sys.env.get("GITEA_USERNAME")
  token <- sys.env.get("GITEA_TOKEN")
} yield Credentials("Gitea API", "gitea.local.vgerasimov.dev", username, token)

import Tasks.generateSequencers

val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.8.3",
    organization := "dev.vgerasimov",
    name := "slowparse",
    version := artifactVersion,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    publishM2Configuration := publishM2Configuration.value.withOverwrite(true),
    resolvers += "gitea" at giteaMaven,
    publishTo := Some("gitea" at giteaMaven),
    publishMavenStyle := true,
    credentials ++= giteaCredentials,
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
