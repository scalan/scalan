lazy val buildSettings = Seq(
  scalaVersion := "2.11.8",
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
    "org.scalatest" %% "scalatest" % "2.2.6" % Test,
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

lazy val backendSettings = commonSettings ++ Defaults.itSettings ++
  Seq(
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "it",
    javaOptions in IntegrationTest ++=
      Seq("-Xmx3g", "-XX:PermSize=384m", "-XX:MaxPermSize=384m", "-XX:ReservedCodeCacheSize=384m"),
    parallelExecution in IntegrationTest := false,
    fork in IntegrationTest := true)

lazy val lmsBackendSettings = backendSettings ++ scalaVirtualizedSettings ++ Seq(
    libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "0.9.1-SNAPSHOT",
    // we know we use LMS snapshot here, ignore it
    releaseSnapshotDependencies := Seq.empty)

lazy val allConfigDependency = "compile->compile;test->test"

cancelable in Global := true

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
      "cglib" % "cglib" % "3.2.0",
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

lazy val lmsBackendCore = Project("scalan-lms-backend-core", file("lms-backend") / "core")
  .dependsOn(core % "compile->compile;it->test")
  .configs(IntegrationTest).settings(lmsBackendSettings)

lazy val lmsBackendCollections = Project("scalan-lms-backend-collections", file("lms-backend") / "collections")
  .dependsOn(lmsBackendCore, collections % "compile->compile;it->test")
  .configs(IntegrationTest).settings(lmsBackendSettings)

lazy val lmsBackendLinAlg = Project("scalan-lms-backend-linear-algebra", file("lms-backend") / "linear-algebra")
  .dependsOn(lmsBackendCollections, linalg % "compile->compile;it->test")
  .configs(IntegrationTest).settings(lmsBackendSettings)

lazy val lmsBackendPointers = Project("scalan-lms-backend-pointers", file("lms-backend") / "pointers")
  .dependsOn(lmsBackendCore, pointers % "compile->compile;it->test")
  .configs(IntegrationTest).settings(lmsBackendSettings)

// contains the integration tests for library modules which don't have their own backend module
// and tests which use multiple modules together
lazy val lmsBackendIT = Project("scalan-lms-backend-tests", file("lms-backend") / "tests")
  .dependsOn(lmsBackendCollections % "compile->compile;it->it", lmsBackendLinAlg % "compile->compile;it->it", graphs % "compile->compile;it->test", lmsBackendPointers % "compile->compile;it->it", effects % "compile->compile;it->test")
  .configs(IntegrationTest).settings(lmsBackendSettings)

lazy val luaBackendCore = Project("scalan-lua-backend-core", file("lua-backend") / "core").
  dependsOn(core % "compile->compile;it->test").
  configs(IntegrationTest).settings(backendSettings).settings(
    libraryDependencies += "org.luaj" % "luaj-jse" % "3.0.1"
  )

lazy val root = Project("scalan", file("."))
  .aggregate(common, meta, core,
    collections, linalg, graphs, pointers, effects,
    lmsBackendCore, lmsBackendCollections, lmsBackendLinAlg, lmsBackendPointers, lmsBackendIT,
    luaBackendCore)
  .settings(buildSettings, publishArtifact := false)
