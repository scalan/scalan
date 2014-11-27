package scalan.frontend

import ch.epfl.yinyang.TypeTreeTransformation
import ch.epfl.yinyang.typetransformers.TypeTransformer

import scala.reflect.macros.blackbox

trait SSTransformer[C <: blackbox.Context] extends TypeTreeTransformation {

  // MacroModule
  type Ctx = C
  //val c: Ctx = ctx

  import c.universe._

  // TypeTransformation
  //val typeTransformer: TypeTransformer[c.type] = ???

  // TransformationUtils
  //def debugLevel: Int = ???

  // Utils
  override val className = "generated$transformer"

  def transform(block: Tree): Tree =
    TypeTreeTransformer(block)

  def apply[T](block: c.Expr[T]): c.Expr[T] = {

    val t = transform(block.tree)
    c.Expr[T](t)
  }
}

object SSTransformer {

  def apply[C <: blackbox.Context](ctx: C)(
    tpeTransformer: TypeTransformer[ctx.type],
    debugLvl: Int): SSTransformer[ctx.type] =

    new SSTransformer[ctx.type] {
      val c: Ctx = ctx

      val typeTransformer = tpeTransformer
      typeTransformer.className = className

      val debugLevel = debugLvl

    }
}
