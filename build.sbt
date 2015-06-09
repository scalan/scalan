lazy val buildSettings = Seq(
  scalaVersion := "2.11.6",
  organization := "com.huawei.scalan",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-feature",
    "-Ywarn-adapted-args",
    "-Ywarn-inaccessible",
    "-Ywarn-nullary-override",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:experimental.macros"))

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
    "com.github.axel22" %% "scalameter" % "0.5-M2" % "test",
    "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
    // otherwise scala-logging-slf4j pulls 2.11.0
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "ch.qos.logback" % "logback-classic" % "1.1.2"),
  parallelExecution in Test := false,
  publishArtifact in Test := true,
  publishArtifact in(Test, packageDoc) := false,
  test in assembly := {}
)

lazy val commonSettings = buildSettings ++ testSettings

lazy val allConfigDependency = "compile->compile;test->test"

// projects

lazy val common = Project("scalan-common", file("common"))
  .settings(commonSettings)

lazy val meta = Project("scalan-meta", file("meta"))
  .dependsOn(common % allConfigDependency)
  .settings(
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    fork in Test := true,
    fork in run := true)

lazy val core = Project("scalan-core", file("core"))
  .dependsOn(common % allConfigDependency)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.0.0",
      "cglib" % "cglib" % "3.1",
      "org.objenesis" % "objenesis" % "2.1"))

lazy val library = Project("scalan-library", file("library"))
  .dependsOn(core % allConfigDependency)
  .settings(commonSettings)

lazy val backend = Project("lms-backend", file("lms-backend"))
  .dependsOn(library % allConfigDependency)
  .configs(IntegrationTest)
  .settings(
    commonSettings,
    Defaults.itSettings,
    scalaVersion := sys.env.getOrElse("SCALA_VIRTUALIZED_VERSION", "2.11.2"),
    scalaOrganization := "org.scala-lang.virtualized",
    libraryDependencies ++= Seq(
      "EPFL" %% "lms_local" % "0.3-SNAPSHOT",
      // old version of ScalaTest used in LMS can lead to ClassCastException
      "EPFL" %% "lms_local" % "0.3-SNAPSHOT" classifier "tests" exclude("org.scalatest", "scalatest_2.11"),
      "org.scala-lang.virtualized" % "scala-library" % scalaVersion.value,
      "org.scala-lang.virtualized" % "scala-compiler" % scalaVersion.value),
    // we know we use LMS snapshot here, ignore it
    releaseSnapshotDependencies := Seq.empty,
    fork in IntegrationTest := true
    //fork in ItTest := true
  )

lazy val root = Project("scalan", file("."))
  .aggregate(common, meta, core, library, backend)
  .settings(
    commonSettings,
    publishArtifact := false)
