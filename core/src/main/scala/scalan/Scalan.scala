package scalan

import scalan.collections.{ListOpsExp, ListOpsSeq, ListOps}
import scalan.compilation.GraphVizExport
import scalan.primitives._
import scalan.arrays._
import scalan.seq.BaseSeq
import scalan.staged.{BaseExp, Expressions, Transforming}

trait Scalan
  extends Base
  with Elems
  with Views
  with Proxy
  with Tuples
  with Loops
  with TypeSum
  with UnBinOps
  with LogicalOps
  with OrderingOps
  with Equal
  with NumericOps
  with StringOps
  with MathOps
  with Functions
  with IfThenElse
  with Blocks
  with Monoids
  with ArrayOps
  with ArrayViews

trait ScalanDsl
  extends Scalan
  with ListOps

trait ScalanSeq
  extends Scalan
  with BaseSeq
  with ElemsSeq
  with ViewsSeq
  with ProxySeq
  with TuplesSeq
  with LoopsSeq
  with TypeSumSeq
  with UnBinOpsSeq
  with NumericOpsSeq
  with FunctionsSeq
  with IfThenElseSeq
  with BlocksSeq
  with MonoidsSeq
  with ArrayOpsSeq
  with ArrayViewsSeq

trait ScalanCtxSeq
  extends ScalanDsl
  with ScalanSeq
  with ListOpsSeq

trait ScalanExp
  extends Scalan
  with BaseExp
  with ElemsExp
  with ViewsExp
  with ProxyExp
  with TuplesExp
  with LoopsExp
  with TypeSumExp
  with UnBinOpsExp
  with EqualExp
  with NumericOpsExp
  with FunctionsExp
  with IfThenElseExp
  with BlocksExp
  with Transforming
  with ArrayOpsExp
  with ArrayViewsExp

trait ScalanCtxExp
  extends ScalanDsl
  with ScalanExp
  with Expressions
  with GraphVizExport
  with ListOpsExp
