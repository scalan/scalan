//libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.5.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value 

fork in Test := true

fork in run := true