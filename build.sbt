lazy val buildSettings = Seq(
  scalaVersion := "2.11.8",
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
    "-language:experimental.macros"),
  publishTo := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  },
  // do not publish docs for snapshot versions
  publishArtifact in (Compile, packageDoc) := !version.value.trim.endsWith("SNAPSHOT"))

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test,
    // TODO separate benchmark configuration, see https://github.com/scalameter/scalameter-examples/blob/master/basic-with-separate-config/build.sbt
    "com.storm-enroute" %% "scalameter" % "0.6" % Test),
  parallelExecution in Test := false,
  publishArtifact in Test := true,
  publishArtifact in (Test, packageSrc) := true,
  publishArtifact in (Test, packageDoc) := false,
  test in assembly := {})

lazy val commonSettings = buildSettings ++ testSettings

lazy val backendSettings = commonSettings ++ Defaults.itSettings ++
  Seq(
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "it",
    javaOptions in IntegrationTest ++=
      Seq("-Xmx3g", "-XX:PermSize=384m", "-XX:MaxPermSize=384m", "-XX:ReservedCodeCacheSize=384m"),
    parallelExecution in IntegrationTest := false,
    fork in IntegrationTest := true)

lazy val allConfigDependency = "compile->compile;test->test"

cancelable in Global := true

// projects

lazy val core = Project("scalan-core", file("core"))
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
      // otherwise scala-logging-slf4j pulls 2.11.0
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai" %% "shapeless" % "2.2.5",
      "cglib" % "cglib" % "3.2.0",
      "org.objenesis" % "objenesis" % "2.2"))

lazy val root = Project("scalan", file("."))
  .aggregate(core)
  .settings(buildSettings, publishArtifact := false)
