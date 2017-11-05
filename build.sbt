
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val buildSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "com.huawei.scalan",
  javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
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
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    // TODO separate benchmark configuration, see https://github.com/scalameter/scalameter-examples/blob/master/basic-with-separate-config/build.sbt
    "com.storm-enroute" %% "scalameter" % "0.6" % Test),
  parallelExecution in Test := false,
  baseDirectory in Test := file("."),
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

lazy val itSettings = commonSettings ++ Defaults.itSettings ++
  Seq(
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "it",
    javaOptions in IntegrationTest ++=
      Seq("-Xmx3g", "-XX:PermSize=384m", "-XX:MaxPermSize=384m", "-XX:ReservedCodeCacheSize=384m"),
    parallelExecution in IntegrationTest := false,
    fork in IntegrationTest := true)

lazy val lmsBackendSettings = itSettings ++ scalaVirtualizedSettings ++ Seq(
    libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "0.9.1-SNAPSHOT",
    // we know we use LMS snapshot here, ignore it
    releaseSnapshotDependencies := Seq.empty)

def libraryDefSettings(scalanizerOption: String) = commonSettings ++ Seq(
  scalacOptions ++= Seq(
          "-Xplugin:scalanizer/target/scala-2.11/scalanizer-assembly-0.3.0-SNAPSHOT.jar"
          , s"-P:scalanizer:module=$scalanizerOption"
    //    , "-Xgenerate-phase-graph"
  )
)

lazy val allConfigDependency = "compile->compile;test->test"

cancelable in Global := true

lazy val common = Project("scalan-common", file("common"))
  .settings(commonSettings,
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "commons-io" % "commons-io" % "2.5"

//    "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.9.1"
  ))

lazy val meta = Project("scalan-meta", file("meta"))
  .dependsOn(common % allConfigDependency)
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.github.kxbmap" %% "configs-java7" % "0.3.0"
      ),
    fork in Test := true,
    fork in run := true)

lazy val scalanizer = Project("scalanizer", file("scalanizer"))
  .dependsOn(meta)
  .settings(commonSettings,
    publishArtifact in (Compile, packageBin) := false,
    assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
    artifact in (Compile, assembly) := {
      val art = (artifact in (Compile, assembly)).value
      art.copy(classifier = Some("assembly"))
    },
    addArtifact(artifact in (Compile, assembly), assembly)
  )

lazy val libraryapi = Project("library-api", file("library-api"))
  .dependsOn(meta, scalanizer)
  .settings(libraryDefSettings("library-api"),
    libraryDependencies ++= Seq())

lazy val libraryimpl = Project("library-impl", file("library-impl"))
  .dependsOn(meta, scalanizer, libraryapi % allConfigDependency)
  .settings(libraryDefSettings("library-impl"),
    libraryDependencies ++= Seq())

lazy val library = Project("library", file("library"))
  .dependsOn(common % allConfigDependency, core % allConfigDependency, libraryimpl)
  .settings(libraryDefSettings("library"),
    libraryDependencies ++= Seq())

lazy val core = Project("scalan-core", file("core"))
  .dependsOn(common % allConfigDependency, meta)
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "cglib" % "cglib" % "3.2.3",
      "org.objenesis" % "objenesis" % "2.4",
      "com.github.kxbmap" %% "configs-java7" % "0.3.0"
    ))

lazy val kotlinBackend = Project("scalan-kotlin-backend", file("kotlin-backend")).
  dependsOn(common % allConfigDependency, core % allConfigDependency, library)
  .configs(IntegrationTest)
  .settings(itSettings)
  .settings(
    //    libraryDependencies += "org.luaj" % "luaj-jse" % "3.0.1"
  )

lazy val toolkit = Project("scalan-toolkit", file("toolkit")).
  dependsOn(common % allConfigDependency, meta % allConfigDependency, core % allConfigDependency)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq("io.spray" %%  "spray-json" % "1.3.3")
  )

lazy val root = Project("scalan", file("."))
  .aggregate(common, meta, scalanizer, libraryapi, libraryimpl, core, library, kotlinBackend)
  .settings(buildSettings, publishArtifact := false)

//lazy val collections = Project("scalan-collections", file("collections"))
//  .dependsOn(core % allConfigDependency)
//  .settings(commonSettings)

//lazy val linalg = Project("scalan-linear-algebra", file("linear-algebra"))
//  .dependsOn(collections % allConfigDependency)
//  .settings(commonSettings)

//lazy val graphs = Project("scalan-graphs", file("graphs"))
//  .dependsOn(collections % allConfigDependency)
//  .settings(commonSettings)

//lazy val pointers = Project("scalan-pointers", file("pointers"))
//  .dependsOn(core % allConfigDependency)
//  .settings(commonSettings)

//lazy val effects = Project("scalan-effects", file("effects"))
//  .dependsOn(collections % allConfigDependency)
//  .settings(commonSettings)

//lazy val lmsBackendCore = Project("scalan-lms-backend-core", file("lms-backend") / "core")
//  .dependsOn(core % "compile->compile;it->test")
//  .configs(IntegrationTest).settings(lmsBackendSettings)

//lazy val lmsBackendCollections = Project("scalan-lms-backend-collections", file("lms-backend") / "collections")
//  .dependsOn(lmsBackendCore, collections % "compile->compile;it->test")
//  .configs(IntegrationTest).settings(lmsBackendSettings)

//lazy val lmsBackendLinAlg = Project("scalan-lms-backend-linear-algebra", file("lms-backend") / "linear-algebra")
//  .dependsOn(lmsBackendCollections, linalg % "compile->compile;it->test")
//  .configs(IntegrationTest).settings(lmsBackendSettings)

//lazy val lmsBackendPointers = Project("scalan-lms-backend-pointers", file("lms-backend") / "pointers")
//  .dependsOn(lmsBackendCore, pointers % "compile->compile;it->test")
//  .configs(IntegrationTest).settings(lmsBackendSettings)

// contains the integration tests for library modules which don't have their own backend module
// and tests which use multiple modules together
//lazy val lmsBackendIT = Project("scalan-lms-backend-tests", file("lms-backend") / "tests")
//  .dependsOn(lmsBackendCollections % "compile->compile;it->it", lmsBackendLinAlg % "compile->compile;it->it", graphs % "compile->compile;it->test", lmsBackendPointers % "compile->compile;it->it", effects % "compile->compile;it->test")
//  .configs(IntegrationTest).settings(lmsBackendSettings)

//lazy val luaBackendCore = Project("scalan-lua-backend-core", file("lua-backend") / "core").
//  dependsOn(core % "compile->compile;it->test").
//  configs(IntegrationTest).settings(itSettings).settings(
//    libraryDependencies += "org.luaj" % "luaj-jse" % "3.0.1"
//  )

lazy val extraClassPathTask = TaskKey[String]("extraClassPath") // scalan.plugins.extraClassPath

// Requires luaBackendCore and lmsBackendLinAlg to be compiled but not on classpath,
// to demonstrate plugin system
// FIXME generates application.scala, not .conf, see https://github.com/sbt/sbt-buildinfo/issues/95
//lazy val examples = Project("scalan-examples", file("examples")).
//  dependsOn(core % "compile->compile;test->test;it->test", linalg).configs(IntegrationTest).settings(itSettings).
//  enablePlugins(BuildInfoPlugin).settings(
//    buildInfoObject := "application",
//    // buildInfoPackage := "", uncomment after sbt-buildinfo includes https://github.com/sbt/sbt-buildinfo/pull/94
//    buildInfoUsePackageAsPath := true,
//    buildInfoKeys := Seq(extraClassPathTask),
//    buildInfoRenderer := ConfBuildInfoRenderer(buildInfoOptions.value)
//  ).
//  settings {
//    (compile in Compile) <<= (compile in Compile).dependsOn(compile in (lmsBackendCore, Compile), compile in (luaBackendCore, Compile))
//  }

// TODO need to also handle internal dependencies
//extraClassPathTask in examples := (externalDependencyClasspath in (core, Compile), externalDependencyClasspath in (luaBackendCore, Compile), packageBin in (luaBackendCore, Compile), externalDependencyClasspath in (lmsBackendCore, Compile), packageBin in (lmsBackendCore, Compile)).map {
//  (coreDeps, luaBackendDeps, luaBackendJar, lmsBackendDeps, lmsBackendJar) =>
//    val luaDeps = luaBackendDeps.diff(coreDeps).map(_.data)
//    val lmsDeps = lmsBackendDeps.diff(coreDeps).map(_.data)
//    val files = (luaDeps ++ lmsDeps ++ List(luaBackendJar, lmsBackendJar)).map(_.getAbsolutePath).distinct
//    files.mkString(java.io.File.pathSeparator)
//}.value


