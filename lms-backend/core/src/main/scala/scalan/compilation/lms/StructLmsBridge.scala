package scalan.compilation.lms

import scala.reflect.RefinedManifest
import scala.reflect.runtime.universe._
import scalan.util.ParamMirror

trait StructLmsBridge extends LmsBridge {
  import scalan._

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: FieldApply[_] => "field"
    case _ => super.lmsMethodName(d, primitiveName)
  }

  override protected def extractParams(d: Def[_], paramMirrors: List[ParamMirror]) = d match {
    case fa: FieldApply[_] =>
      List(fa.struct, fa.fieldName, fa.selfType)
    case _ =>
      super.extractParams(d, paramMirrors)
  }

  private def refinedManifestForStruct[A <: Struct](se: StructElem[A]) = {
    val classTag = TagImplicits.typeTagToClassTag(se.tag)
    new RefinedManifest[A] {
      def runtimeClass = classTag.runtimeClass
      val fields = se.fields.toList.map {
        case (name, elem) => (name, elemToManifest(elem))
      }
      override def toString = runtimeClass + fields.map { case(n, t) => s"val $n: $t" }.mkString("{", "; ", "}")
      // should we override equals? It only compares runtimeClass, not fields
    }
  }

  def structName(se: StructElem[_ <: Struct]) = {
    val m = refinedManifestForStruct(se)
    lms.structName(m)
  }

  override protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case struct: AbstractStruct[a] =>
      // Scala fails to infer a <: Struct bound
      implicit val mA = refinedManifestForStruct(struct.asInstanceOf[AbstractStruct[a with Struct]].selfType)
      val lmsTag = lms.AnonTag(mA)
      val lmsFields = struct.fields.map { case (name, value) => (name, m.symMirrorUntyped(value)) }
      val lmsStruct = lms.struct(lmsTag, lmsFields)
      m.addSym(sym, lmsStruct)
    case _ => super.transformDef(m, g, sym, d)
  }

  override def elemToManifest[T](elem: Elem[T]): Manifest[_] = elem match {
    case se: StructElem[_] => refinedManifestForStruct(se.asInstanceOf[StructElem[_ <: Struct]])
    case _ => super.elemToManifest(elem)
  }
}
