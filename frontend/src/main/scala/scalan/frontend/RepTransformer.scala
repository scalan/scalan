package scalan.frontend

import ch.epfl.yinyang.typetransformers.{PolyTransformerLike, TypeTransformer, RepTransformerLike}
import scala.reflect.macros.blackbox

class RepTransformer[C <: blackbox.Context](ctx: C) extends TypeTransformer(ctx)
    with RepTransformerLike[C]
    with PolyTransformerLike[C] {

  def transform(ctx: TypeContext, t: c.universe.Type): c.universe.Tree =
    constructRepTree(ctx, t)
}
