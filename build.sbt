scalaVersion := "2.10.3"

organization := "com.huawei"

name := "scalan-lite"

version := "0.1"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation", 
  "-Xlint",
  "-feature", 
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:experimental.macros")

//libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.5.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "EPFL" % "lms_2.10" % "0.3-SNAPSHOT")

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.10.3" % "2.0.0-M1"
)

//testOptions in Test += Tests.Argument("-oD")

