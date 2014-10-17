package scalan

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
  with OrderingOps
  with NumericOps
  with Equal
  with MathOps
  with LogicalOps
  with FractionalOps
  with Functions
  with IfThenElse
  with Monoids
  with ArrayOps
  with ArrayViews

trait ScalanDsl
  extends Scalan

trait ScalanSeq
  extends Scalan
  with BaseSeq
  with ElemsSeq
  with ViewsSeq
  with ProxySeq
  with TuplesSeq
  with LoopsSeq
  with TypeSumSeq
  with OrderingOpsSeq
  with NumericOpsSeq
  with EqualSeq
  with MathOpsSeq
  with LogicalOpsSeq
  with FractionalOpsSeq
  with FunctionsSeq
  with IfThenElseSeq
  with ArrayOpsSeq
  with ArrayViewsSeq

trait ScalanCtxSeq
  extends ScalanDsl
  with ScalanSeq

trait ScalanExp
  extends Scalan
  with BaseExp
  with ElemsExp
  with ViewsExp
  with ProxyExp
  with TuplesExp
  with LoopsExp
  with TypeSumExp
  with OrderingOpsExp
  with NumericOpsExp
  with EqualExp
  with MathOpsExp
  with LogicalOpsExp
  with FractionalOpsExp
  with FunctionsExp
  with IfThenElseExp
  with Transforming
  with ArrayOpsExp
  with ArrayViewsExp

trait ScalanCtxExp
  extends ScalanDsl
  with ScalanExp
  with Expressions
  with GraphVizExport
