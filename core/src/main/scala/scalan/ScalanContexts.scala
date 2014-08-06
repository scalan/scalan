package scalan

import scalan.codegen.GraphVizExport
import scalan.primitives._
import scalan.seq.BaseSeq
import scalan.staged.{BaseExp, Expressions, Transforming}

trait Scalan
  extends Base
  with Elems
  //     with Descs
  with Views
  with ProxyBase
  //     with Chunks
  //     with Sets
  //     with Zipping
  with Tuples
  with Loops
  with TypeSum
  with NumericOps
  with MathOps
  with LogicalOps
  with OrderingOps
  with FractionalOps
  with Functions
  with IfThenElse
  with Equal
  with Monoids

trait ScalanDsl
  extends Scalan

trait ScalanSeq
  extends Scalan
  with BaseSeq
  with ElemsSeq
  //  with DescsSeq
  //  with SeqSets
  with ViewsSeq
  with ProxySeq
  with TuplesSeq
  with LoopsSeq
  with TypeSumSeq
  with FunctionsSeq
  with IfThenElseSeq
  with OrderingOpsSeq
  with NumericOpsSeq
  with EqualSeq
  with MathOpsSeq
  with LogicalOpsSeq
  with FractionalOpsSeq

trait ScalanSeqImplementation
  extends ScalanDsl
  with ScalanSeq

// with StringOpsSeqx

trait ScalanStaged
  extends Scalan
  with BaseExp
  with TuplesExp
  with LoopsExp
  with TypeSumExp
  with ElemsExp
  //  with DescsExp
  //  with StagedSets
  with ViewsExp
  with ProxyExp
  with Transforming
  with NumericOpsExp
  with EqualExp
  with MathOpsExp
  with LogicalOpsExp
  with OrderingOpsExp
  with FractionalOpsExp
  with FunctionsExp
  with IfThenElseExp

trait ScalanStagedImplementation
  extends ScalanDsl
  with ScalanStaged
  with EqualExp
  with Expressions
  with GraphVizExport

trait ScalanCtxSeq extends ScalanSeqImplementation {}

trait ScalanCtxStaged extends ScalanStagedImplementation {}
