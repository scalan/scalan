scalaVersion := "2.10.4"

organization := "com.huawei"

name := "scalan-meta"

version := "0.1"

//libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.5.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value 

//testOptions in Test += Tests.Argument("-oD")

