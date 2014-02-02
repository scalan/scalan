scalaVersion := "2.10.3"

organization := "com.huawei"

name := "scalan-lite"

version := "0.1"

scalacOptions += "-feature"

//libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.5.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "ch.qos.logback" % "logback-classic" % "1.0.13")

libraryDependencies ++= Seq(
  //"com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
  "com.chuusai" % "shapeless_2.10.3" % "2.0.0-M1" // alternatively ...
)

//testOptions in Test += Tests.Argument("-oD")

