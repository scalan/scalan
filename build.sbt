lazy val buildSettings = Seq(
scalaVersion := "2.11.7",
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
    "org.scalatest" %% "scalatest" % "2.2.5" % Test,
    // TODO separate benchmark configuration, see https://github.com/scalameter/scalameter-examples/blob/master/basic-with-separate-config/build.sbt
    "com.storm-enroute" %% "scalameter" % "0.6" % Test),
  parallelExecution in Test := false,
  publishArtifact in Test := true,
  publishArtifact in (Test, packageSrc) := true,
  publishArtifact in (Test, packageDoc) := false,
  test in assembly := {})

lazy val commonSettings = buildSettings ++ testSettings

lazy val scalaVirtualizedVersion = sys.env.getOrElse("SCALA_VIRTUALIZED_VERSION", "2.11.2")

lazy val scalaVirtualizedSettings = Seq(
  scalaVersion := scalaVirtualizedVersion,
  scalaOrganization := "org.scala-lang.virtualized",
  libraryDependencies ++= Seq(
    "org.scala-lang.virtualized" % "scala-library" % scalaVirtualizedVersion,
    "org.scala-lang.virtualized" % "scala-compiler" % scalaVirtualizedVersion
  )
)

lazy val commonBackendSettings = commonSettings ++ Defaults.itSettings ++
  scalaVirtualizedSettings ++ Seq(
    libraryDependencies ++= Seq(
      "org.scala-lang.lms" %% "lms-core" % "0.9.1-SNAPSHOT",
      "org.scalatest" %% "scalatest" % "2.2.5" % "it"),
    // we know we use LMS snapshot here, ignore it
    releaseSnapshotDependencies := Seq.empty,
    javaOptions in IntegrationTest ++=
      Seq("-Xmx3g", "-XX:PermSize=384m", "-XX:MaxPermSize=384m", "-XX:ReservedCodeCacheSize=384m"),
    parallelExecution in IntegrationTest := false,
    fork in IntegrationTest := true)

lazy val allConfigDependency = "compile->compile;test->test"

// projects

lazy val common = Project("scalan-common", file("common"))
  .settings(commonSettings,
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
    // otherwise scala-logging-slf4j pulls 2.11.0
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "ch.qos.logback" % "logback-classic" % "1.1.3"))

lazy val meta = Project("scalan-meta", file("meta"))
  .dependsOn(common % allConfigDependency)
  .settings(commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    fork in Test := true,
    fork in run := true)

lazy val core = Project("scalan-core", file("core"))
  .dependsOn(common % allConfigDependency, meta)
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.2.5",
      "cglib" % "cglib" % "3.1",
      "org.objenesis" % "objenesis" % "2.2"))

lazy val collections = Project("scalan-collections", file("collections"))
  .dependsOn(core % allConfigDependency)
  .settings(commonSettings)

lazy val linalg = Project("scalan-linear-algebra", file("linear-algebra"))
  .dependsOn(collections % allConfigDependency)
  .settings(commonSettings)

lazy val graphs = Project("scalan-graphs", file("graphs"))
  .dependsOn(collections % allConfigDependency)
  .settings(commonSettings)

lazy val pointers = Project("scalan-pointers", file("pointers"))
  .dependsOn(core % allConfigDependency)
  .settings(commonSettings)

lazy val effects = Project("scalan-effects", file("effects"))
  .dependsOn(collections % allConfigDependency)
  .settings(commonSettings)

lazy val backend = Project("scalan-lms-backend", file("lms-backend"))
  .dependsOn(collections % "compile->compile;it->test", linalg % "compile->compile;it->test", graphs % "compile->compile;it->test", pointers % "compile->compile;it->test", effects % "compile->compile;it->test")
  .configs(IntegrationTest).settings(commonBackendSettings)

lazy val root = Project("scalan", file("."))
  .aggregate(common, meta, core,
    collections, linalg, graphs, pointers, effects,
    backend)
  .settings(buildSettings, publishArtifact := false)
