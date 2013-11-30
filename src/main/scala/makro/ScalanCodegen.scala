/**
 * User: Alexander Slesarenko
 * Date: 11/16/13
 */
package makro

import scalan.codegen.emit.Formatter
import scalan.codegen.emit.ScalaCodeEmitter

trait ScalanCodegen extends ScalanAst with ScalanParsers {

  class EntityFileGenerator(module: EntityModuleDef) {

//    def getBaseTrait = {
//      implicit val f = new Formatter { tabSize = 2 }
//      val  emitter = new ScalaCodeEmitter
//
//      f.toString
//    }
//
    def getSeqTrait: String = {
      val defs = for { c <- module.concreteClasses } yield {
        val isoDefs =
          s"""
           |  implicit def isoObservableImpl[T:Elem]:Iso[ObservableImplData[T], ObservableImpl[T]]
           |    = new ObservableImpl.Iso[T] with SeqIso[ObservableImplData[T], ObservableImpl[T]] { i =>
           |        // should use i as iso reference
           |        override lazy val eB = new SeqViewElem[ObservableImplData[T], ObservableImpl[T]]
           |                                    with ObservableImplElem[T] { val iso = i }
           |      }
          """.stripMargin

        val constrDefs =
          s"""
           |  def mkObservableImpl[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean])
           |    = new ObservableImpl[T](value, index, completed)
           |  def unmkObservableImpl[T:Elem](p: Rep[ObservableImpl[T]])
           |    = Some((p.value, p.index, p.completed))
          """.stripMargin

        s"""$isoDefs \n\n $constrDefs"""
      }

      s"""trait ${module.name}Seq extends ${module.name} { self: ScalanSeq =>
         |  ${defs.mkString("\n")}
         |}
      """.stripMargin
    }

    def getStagedTrait: String = ""
  
    def getFileHeader = {
      val p = "package %s\n\n".format(module.packageName)
      val imports =
        """import scalan.lms.common.ProxyExp
          |import scalan.ScalanDsl
          |import scalan.common.Common
          |import Common._
          |import scalan.sequential.ScalanSeq
          |import scalan.staged.{StagedViews, ScalanStaged}
          |import reflect.SourceContext
          |
          |""".stripMargin
      p + imports
    }
  
    def generateCode: String = {
      val topLevel = List(
        getFileHeader,
        getSeqTrait,
        getStagedTrait
      )
      topLevel.mkString("\n")
    }
  }
}

object ScalanCodegen extends ScalanCodegen

