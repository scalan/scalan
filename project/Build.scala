import sbt._
import sbt.Keys._

object ScalanLiteBuild extends Build {
  version in ThisBuild := "0.1-SNAPSHOT"

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

  val commonSettings = Seq(
    scalaVersion := "2.10.3",
    organization := "com.huawei",
    version := "0.1-SNAPSHOT",
    opts, commonDeps) ++ testSettings

  lazy val ItTest = config("it").extend(Test)

  def itFilter(name: String): Boolean = false // name.contains("ItTests")

  def unitFilter(name: String): Boolean = !itFilter(name)

  lazy val core = Project("scalan-lite", file("core")).configs(ItTest).settings(commonSettings: _*)
  
  lazy val lmsBackend = Project("lms-backend", file("lms-backend")).dependsOn(core % "compile->compile;test->test").configs(ItTest).
    settings(commonSettings: _*).settings(libraryDependencies += "EPFL" % "lms_2.10" % "0.3-SNAPSHOT")
  
  // name to make this the default project
  lazy val root = Project("all", file(".")).aggregate(core, lmsBackend).settings(commonSettings: _*)
}