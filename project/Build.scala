import sbt.Keys._
import sbt._
import sbtassembly.Plugin._
import sbtrelease.ReleasePlugin._

object ScalanBuild extends Build {
  val opts = scalacOptions ++= Seq(
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
    "-language:experimental.macros")

  val commonDeps = libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
    "com.github.axel22" %% "scalameter" % "0.5-M2" % "test",
    "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
    "ch.qos.logback" % "logback-classic" % "1.1.2")

  val testSettings = inConfig(ItTest)(Defaults.testTasks) ++
    inConfig(PerfTest)(Defaults.testTasks ++ baseAssemblySettings) ++ Seq(
    testOptions in Test := Seq(Tests.Filter(x => !itFilter(x))),
    testOptions in ItTest := Seq(Tests.Filter(x => itFilter(x))),
    testFrameworks in PerfTest := Seq(new TestFramework("org.scalameter.ScalaMeterFramework")),
    logBuffered in PerfTest := false,
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in ItTest := false,
    parallelExecution in PerfTest := false,
    fork in PerfTest := true,
    javaOptions in PerfTest ++= Seq("-Xmx30G", "-Xms15G"),
    publishArtifact in Test := true,
    publishArtifact in(Test, packageDoc) := false)

  val commonSettings = Seq(
    scalaVersion := "2.10.4",
    organization := "com.huawei.scalan",
    publishTo := {
      val nexus = "http://10.122.85.37:9081/nexus/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at (nexus + "content/repositories/snapshots"))
      else
        Some("releases" at (nexus + "content/repositories/releases"))
    },
    opts, commonDeps) ++ testSettings ++ assemblySettings ++ releaseSettings

  lazy val ItTest = config("it").extend(Test)

  lazy val PerfTest = config("perf").extend(Test)

  def itFilter(name: String): Boolean = name.contains("ItTests")

  implicit class ProjectExt(p: Project) {
    def allConfigDependency = p % "compile->compile;test->test"

    def addTestConfigsAndCommonSettings =
      p.configs(ItTest, PerfTest).settings(commonSettings: _*)
  }

  lazy val common = project.addTestConfigsAndCommonSettings

  lazy val meta = project.dependsOn(common).addTestConfigsAndCommonSettings

  lazy val core = project.dependsOn(common).addTestConfigsAndCommonSettings.settings(
    libraryDependencies ++= Seq(
      "com.chuusai" % "shapeless_2.10.4" % "2.0.0",
      "cglib" % "cglib" % "3.1",
      "org.objenesis" % "objenesis" % "2.1"))

  lazy val coreDep = core.allConfigDependency

  lazy val ce = Project("community-edition", file("community-edition")).dependsOn(coreDep).
    addTestConfigsAndCommonSettings

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.2")

  val lms = "EPFL" % "lms_local_2.10" % "0.3-SNAPSHOT"

  lazy val lmsBackend = Project("lms-backend", file("lms-backend")).dependsOn(coreDep, ce.allConfigDependency).
    addTestConfigsAndCommonSettings.settings(
      libraryDependencies ++= Seq(lms,
        lms classifier "tests",
        "org.scala-lang.virtualized" % "scala-library" % virtScala,
        "org.scala-lang.virtualized" % "scala-compiler" % virtScala),
      scalaOrganization := "org.scala-lang.virtualized",
      scalaVersion := virtScala,
      // we know we use LMS snapshot here, ignore it
      ReleaseKeys.snapshotDependencies := Seq.empty)

  // name to make this the default project
  lazy val root = Project("scalan", file(".")).aggregate(common, meta, core, ce, lmsBackend).
    configs(ItTest, PerfTest).settings(commonSettings: _*).
    // don't publish or release the aggregate project itself
    settings(publishArtifact := false, publish := {}, publishLocal := {})
}
