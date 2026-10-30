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

/** stryker4s `thresholds.break` cannot require a 100% score: this check can. */
lazy val mutantsCheck =
    taskKey[Unit]("Fails unless the latest stryker4s report has no surviving or uncovered mutant")

// `sbt mutationTest`: mutation testing, failing unless every mutant is killed
addCommandAlias("mutationTest", ";stryker ;jvm/mutantsCheck")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  libraryDependencies ++= Seq(
    "org.scalus" %% "scalus" % scalusVersion,
    "org.scalus" %% "scalus-cardano-ledger" % scalusVersion,
    "org.scalus" %% "scalus-testkit" % scalusVersion % Test,
    "org.scalatest" %% "scalatest" % "3.2.20" % Test,
    "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test,
    // the JSON parser Scalus uses, to check the serialise tool's JSON
    "com.lihaoyi" %% "ujson" % "4.4.3" % Test
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
      coverageMinimumBranchTotal := 90,
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
      mutantsCheck := {
          val reports =
              (strykerBaseDir.value / "target" / "stryker4s-report" ** "report.json").get()
          if (reports.isEmpty) sys.error("No stryker4s report: run `sbt stryker` first")
          val report = reports.maxBy(_.lastModified)
          val undetected = """"status"\s*:\s*"(Survived|NoCoverage)"""".r
              .findAllIn(IO.read(report))
              .size
          val html = report.getParentFile / "index.html"
          if (undetected > 0)
              sys.error(s"$undetected mutants survived or were not tested: see $html")
          streams.value.log.info(s"Every mutant was killed: see $html")
      },
      publish / skip := true
    )
