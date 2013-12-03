scalaVersion := "2.10.3"

organization := "com.huawei"

name := "scalan-lite"

version := "0.1"

//libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.5.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies ++= Seq(
  //"com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
  "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1" // alternatively ...
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

//testOptions in Test += Tests.Argument("-oD")

