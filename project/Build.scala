import sbt._
import sbt.Keys._

object ScalanLiteBuild extends Build {
  scalaVersion in ThisBuild := "2.10.3"

  organization in ThisBuild := "com.huawei"

  version in ThisBuild := "0.1-SNAPSHOT"

  scalacOptions in ThisBuild ++= Seq(
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:experimental.macros")

  val commonDeps = libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
    "ch.qos.logback" % "logback-classic" % "1.0.13",
    "com.chuusai" % "shapeless_2.10.3" % "2.0.0-M1")

  val testSettings = Seq(
    testOptions in Test ++= Seq(Tests.Filter(unitFilter)),
    testOptions in ItTest ++= Seq(Tests.Filter(itFilter)),
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in ItTest := false)

  lazy val ItTest = config("it").extend(Test)

  def itFilter(name: String): Boolean = false // name.contains("ItTests")

  def unitFilter(name: String): Boolean = !itFilter(name)

  lazy val core = Project("scalan-lite", file("core")).configs(ItTest).settings(testSettings: _*).settings(commonDeps)
  
  lazy val lmsBackend = Project("lms-backend", file("lms-backend")).dependsOn(core % "compile->compile;test->test").configs(ItTest).
    settings(testSettings: _*).settings(commonDeps).settings(libraryDependencies += "EPFL" % "lms_2.10" % "0.3-SNAPSHOT")
  
  // name to make this the default project
  lazy val root = Project("all", file(".")).aggregate(core, lmsBackend).settings(testSettings: _*)
}