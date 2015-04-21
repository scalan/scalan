package scalan.compilation.lms.source2bin

/**
 * Created by adel on 4/8/15.
 */

case class SbtConfig(mainPack: Option[String] = None,
                     extraClasses : Seq[String] = Seq.empty[String],
                     resources : Seq[String] = Seq.empty[String],
                     mainClassSimpleName: String = "run",
                     commands: Seq[String] = Seq("clean", "compile"),
                     toSystemOut: Boolean = true)