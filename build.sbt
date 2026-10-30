val scalusVersion = "1.4.1"

ThisBuild / scalaVersion := "3.3.8"
ThisBuild / organization := "fida"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
    .settings(
      name := "fida-contracts",
      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
      addCompilerPlugin(
        "org.scalus" % "scalus-plugin" % scalusVersion cross CrossVersion.full
      ),
      libraryDependencies ++= Seq(
        "org.scalus" %% "scalus" % scalusVersion,
        "org.scalus" %% "scalus-cardano-ledger" % scalusVersion,
        "org.scalus" %% "scalus-testkit" % scalusVersion % Test,
        "org.scalatest" %% "scalatest" % "3.2.20" % Test,
        "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test
      ),
      Test / fork := true,
      Test / javaOptions ++= Seq("-Xss16m", "-Xmx2g"),
      // `sbt "run -o DIR"`, `sbt "run -n MODULE [-p FILE]"`: the serialise tool, run from the
      // project directory with the stack the UPLC lowering needs and its output unprefixed
      Compile / run / mainClass := Some("fida.serialise.Main"),
      Compile / run / fork := true,
      Compile / run / javaOptions ++= Seq("-Xss16m"),
      Compile / run / outputStrategy := Some(StdoutOutput)
    )
