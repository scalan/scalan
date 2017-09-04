package scalan

import scalan.compilation.GraphVizExport
import scalan.primitives._
import scalan.staged.{BaseExp, Expressions, Transforming, TransformingExp}
import scalan.util.{ExceptionsDsl}

abstract class ScalanExp
  extends BaseExp
  with Debugging
  with TypeDescsExp
  with TypeWrappersExp

  with ProxyExp
  with TuplesExp
  with LoopsExp
  with TypeSumExp
  with NumericOpsExp
  with UnBinOpsExp
  with LogicalOpsExp
  with OrderingOpsExp
  with MathOps
  with Monoids
  with EqualExp
  with UniversalOpsExp
  with FunctionsExp
  with IfThenElseExp
  with BlocksExp
  with PatternMatchingExp
  with TransformingExp
  with AnalyzingExp
  with ExceptionsExp
  with StringOpsExp
  with EffectsExp
  with MetadataExp
  with RewriteRulesExp
  with GraphVizExport
  with ViewsDsl
  with ThunksExp
  with Structs
  with ConvertersDsl


trait ScalanDsl
  extends ScalanExp
    with ExceptionsDsl

class ScalanDslExp
extends ScalanExp
  with ScalanDsl
  with Expressions
  with ExceptionsDsl

class ScalanDslStd
extends ScalanExp
  with ScalanDsl
  with Expressions
  with ExceptionsDsl
