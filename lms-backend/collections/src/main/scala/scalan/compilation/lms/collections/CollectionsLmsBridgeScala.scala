package scalan.compilation.lms.collections

import scalan.collections.{CollectionsDslExp, SeqsScalaMethodMapping}
import scalan.collections.impl.CollectionsExp
import scalan.compilation.lms.CoreLmsBridgeScala

trait CollectionsLmsBridgeScala extends CoreLmsBridgeScala with SeqsScalaMethodMapping {
  override val scalan: CollectionsDslExp
  import scalan._

  // Removing causes MethodCallItTests.Class Mapping to fail, error is that Scala field arr is not represented
  // as a Java field (because ExpCollectionOverArray inherits it from CollectionOverArray).
  // TODO implement this case generically
  override protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case u: CollectionsExp#ExpCollectionOverArray[_] =>
      val exp = Manifest.classType(u.getClass) match {
        case (mA: Manifest[a]) =>
          lms.newObj[a]("scalan.imp.ArrayImp", Seq(m.symMirrorUntyped(u.arr.asInstanceOf[Exp[_]])), true)(mA)
      }
      m.addSym(sym, exp)
    case _ => super.transformDef(m, g, sym, d)
  }
}
