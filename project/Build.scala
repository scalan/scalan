import sbt._
import sbt.Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object ScalanBuild extends Build {
  val opts = scalacOptions ++= Seq(
    // "-unchecked",
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
    "org.scalatest" %% "scalatest" % "2.1.6" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
    "com.github.axel22" %% "scalameter" % "0.5-M2" % "test",
    "com.typesafe" %% "scalalogging-slf4j" % "1.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.2",
    "com.chuusai" % "shapeless_2.10.4" % "2.0.0",
    "cglib" % "cglib" % "3.1",
    "org.objenesis" % "objenesis" % "2.1")

  val testSettings = Seq(
    testOptions in Test := Seq(Tests.Filter(x => !itFilter(x))),
    testOptions in ItTest := Seq(Tests.Filter(x => itFilter(x))),
    testFrameworks in PerfTest := Seq(new TestFramework("org.scalameter.ScalaMeterFramework")),
    logBuffered in PerfTest := false,
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in ItTest := false,
    parallelExecution in PerfTest := false,
    fork in PerfTest := true,
    javaOptions in PerfTest ++= Seq("-Xmx30G", "-Xms15G"))

  val commonSettings = inConfig(ItTest)(Defaults.testTasks) ++ 
    inConfig(PerfTest)(Defaults.testTasks ++ baseAssemblySettings) ++ Seq(
      scalaVersion := "2.10.4",
      organization := "com.huawei.scalan",
      version := "0.1-SNAPSHOT",
      opts, commonDeps) ++ 
    testSettings ++ assemblySettings

  lazy val ItTest = config("it").extend(Test)

  lazy val PerfTest = config("perf").extend(Test)

  def itFilter(name: String): Boolean = name.contains("ItTests")

  implicit class ProjectExt(p: Project) {
    def allConfigs = p % "compile->compile;test->test"
  }

  lazy val core = project.in(file("core")).configs(ItTest, PerfTest).
    settings(commonSettings: _*)

  lazy val coreDep = core.allConfigs

  lazy val ce = Project("community-edition", file("community-edition")).dependsOn(coreDep).configs(ItTest, PerfTest).
    settings(commonSettings: _*)

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.2")

  lazy val lmsBackend = Project("lms-backend", file("lms-backend")).dependsOn(coreDep, ce.allConfigs).configs(ItTest, PerfTest).
    settings(commonSettings: _*).settings(
     libraryDependencies ++= Seq("EPFL" % "lms_local_2.10" % "0.3-SNAPSHOT",
                                 "EPFL" % "lms_local_2.10" % "0.3-SNAPSHOT" classifier "tests",
                                 "org.scala-lang.virtualized" % "scala-library" % virtScala,
                                 "org.scala-lang.virtualized" % "scala-compiler" % virtScala),
     scalaOrganization := "org.scala-lang.virtualized",
     scalaVersion := virtScala
  )
  
  // name to make this the default project
  lazy val root = Project("all", file(".")).aggregate(core, ce, lmsBackend).
    configs(ItTest, PerfTest).settings(commonSettings: _*)
}
