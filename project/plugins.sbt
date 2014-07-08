import sbt._

import sbt.Keys._

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.1")

//addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0")

addSbtPlugin("com.gu" % "sbt-teamcity-test-reporting-plugin" % "1.5")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")