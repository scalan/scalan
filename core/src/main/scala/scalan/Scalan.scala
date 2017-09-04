package scalan

import scalan.compilation.GraphVizExport
import scalan.primitives._
import scalan.staged.{Transforming, Expressions}
import scalan.util.ExceptionsDsl

abstract class Scalan
  extends Base
  with Debugging
  with TypeDescs
  with TypeWrappers
  with Metadata
  with Proxy
  with Tuples
  with Loops
  with TypeSum
  with NumericOps
  with UnBinOps
  with LogicalOps
  with OrderingOps
  with MathOps
  with Monoids
  with Equal
  with UniversalOps
  with Functions
  with IfThenElse
  with Blocks
  with PatternMatching
  with Transforming
  with Analyzing
  with Exceptions
  with StringOps
  with Effects
  with RewriteRules
  with GraphVizExport
  with ViewsDsl
  with Thunks
  with Structs
  with ConvertersDsl

class ScalanDsl
  extends Scalan
  with Expressions
  with ExceptionsDsl
