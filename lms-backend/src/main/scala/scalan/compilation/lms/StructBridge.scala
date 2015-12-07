package scalan.compilation.lms

import scala.reflect.RefinedManifest

trait StructBridge extends LmsBridge {
  import scalan._

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: FieldApply[_] => "field"
    case _ => super.lmsMethodName(d, primitiveName)
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
