package scalan

import scalan.compilation.GraphVizExport
import scalan.primitives._
import scalan.collections._
import scalan.arrays._
import scalan.seq.BaseSeq
import scalan.staged.{BaseExp, Expressions, Transforming}
import scalan.util.{ExceptionsDslExp, ExceptionsDslSeq, ExceptionsDsl/*, Exceptions*/}

trait Scalan
  extends Base
  with Elems
  with Containers
  with BaseTypes
  with Views
  with Proxy
  with Tuples
  with Loops
  with TypeSum
  with UnBinOps
  with LogicalOps
  with OrderingOps
  with NumericOps
  with StringOps
  with Equal
  with MathOps
  with Functions
  with IfThenElse
  with Blocks
  with Monoids
  with Maps
  with ArrayOps
  with ArrayBuffers
  with Exceptions
  with ArrayViews
  with Thunks
  with Effects {

}

trait ScalanDsl
  extends Scalan
  with ListOps
  with ListViews
  with ExceptionsDsl

trait ScalanSeq
  extends Scalan
  with BaseSeq
  with ElemsSeq
  with ContainersSeq
  with BaseTypesSeq
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
  with MapsSeq
  with MonoidsSeq
  with ArrayOpsSeq
  with ArrayBuffersSeq
  with ExceptionsSeq
  with ArrayViewsSeq
  with StringOpsSeq
  with ThunksSeq
  with EffectsSeq

trait ScalanCtxSeq
  extends ScalanDsl
  with ScalanSeq
  with ListOpsSeq
  with ListViewsSeq
  with ExceptionsDslSeq

trait ScalanExp
  extends Scalan
  with BaseExp
  with ElemsExp
  with ContainersExp
  with BaseTypesExp
  with ViewsExp
  with ProxyExp
  with TuplesExp
  with LoopsExp
  with TypeSumExp
  with NumericOpsExp
  with UnBinOpsExp
  with LogicalOpsExp
  with EqualExp
  with FunctionsExp
  with IfThenElseExp
  with BlocksExp
  with MapsExp
  with Transforming
  with ArrayOpsExp
  with ArrayBuffersExp
  with ExceptionsExp
  with ArrayViewsExp
  with StringOpsExp
  with ThunksExp
  with EffectsExp

trait ScalanCtxExp
  extends ScalanDsl
  with ScalanExp
  with Expressions
  with GraphVizExport
  with ListOpsExp
  with ListViewsExp
  with ExceptionsDslExp
