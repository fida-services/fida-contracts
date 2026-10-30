val scalusVersion = "1.4.1"

ThisBuild / scalaVersion := "3.3.8"
ThisBuild / organization := "fida"
ThisBuild / version := "0.1.0-SNAPSHOT"

// Coverage exclusion patterns. Scala 3 matches them against source paths without `.scala`.

/** The on-chain sources: every `@Compile` object and the types they use. */
val onChainSources = ".*/src/main/scala/fida/contract/(?!Scripts$).*"

/** The off-chain sources: script compilation and the serialise tool. */
val offChainSources =
    ".*/src/main/scala/fida/contract/Scripts;.*/src/main/scala/fida/serialise/.*"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  libraryDependencies ++= Seq(
    "org.scalus" %% "scalus" % scalusVersion,
    "org.scalus" %% "scalus-cardano-ledger" % scalusVersion,
    "org.scalus" %% "scalus-testkit" % scalusVersion % Test,
    "org.scalatest" %% "scalatest" % "3.2.20" % Test,
    "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test
  ),
  Test / fork := true,
  Test / javaOptions ++= Seq("-Xss16m", "-Xmx2g")
)

lazy val root = (project in file("."))
    .aggregate(jvm)
    .settings(
      commonSettings,
      name := "fida-contracts",
      addCompilerPlugin(
        "org.scalus" % "scalus-plugin" % scalusVersion cross CrossVersion.full
      ),
      // The Scalus plugin compiles `@Compile` objects after coverage instrumentation and cannot
      // compile the instrumentation calls: the on-chain sources are measured in `jvm` instead.
      coverageExcludedFiles := onChainSources,
      coverageMinimumStmtTotal := 90,
      coverageFailOnMinimum := true,
      // mutation testing runs in `jvm`, where the on-chain code can be mutated
      stryker := (jvm / stryker).evaluated,
      stryker / aggregate := false,
      // `sbt "run -o DIR"`, `sbt "run -n MODULE [-p FILE]"`: the serialise tool, run from the
      // project directory with the stack the UPLC lowering needs and its output unprefixed
      Compile / run / mainClass := Some("fida.serialise.Main"),
      Compile / run / fork := true,
      Compile / run / javaOptions ++= Seq("-Xss16m"),
      Compile / run / outputStrategy := Some(StdoutOutput),
      run / aggregate := false
    )

/** The same sources and tests compiled for the JVM only, without the Scalus plugin, so that
  * scoverage can instrument the on-chain code and stryker4s can mutate it.
  *
  * Tests evaluate the validators as Scala code, which the same tests check in `root` against the
  * compiled UPLC; tests that need compiled scripts are canceled here.
  */
lazy val jvm = (project in file("jvm"))
    .settings(
      commonSettings,
      name := "fida-contracts-jvm",
      sourceDirectory := (ThisBuild / baseDirectory).value / "src",
      // helpers for mutation testing, only in this build
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "main" / "scala",
      Test / javaOptions += "-Dfida.uplc=false",
      coverageExcludedFiles := s"$offChainSources;.*/jvm/src/.*",
      strykerMutate := Seq(
        "src/main/scala/fida/contract/**.scala",
        "!src/main/scala/fida/contract/Scripts.scala"
      ),
      strykerStaticTmpDir := true,
      strykerReporters := Seq("console", "html", "json"),
      publish / skip := true
    )
