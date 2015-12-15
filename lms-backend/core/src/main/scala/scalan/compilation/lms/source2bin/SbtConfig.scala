package scalan.compilation.lms.source2bin

import scala.util.Properties

/**
 * Created by adel on 4/8/15.
 */

case class SbtConfig(scalaVersion: String = SbtConfig.defaultScalaVersion,
                     mainPack: Option[String] = None,
                     extraClasses : Seq[String] = Seq.empty[String],
                     resources : Seq[String] = Seq.empty[String],
                     mainClassSimpleName: String = "run",
                     commands: Seq[String] = Seq("clean", "compile"),
                     toSystemOut: Boolean = true) {
  def scalaBinaryVersion = scalaVersion.substring(0, scalaVersion.lastIndexOf("."))
}

object SbtConfig {
  val defaultScalaVersion = Properties.versionNumberString
}