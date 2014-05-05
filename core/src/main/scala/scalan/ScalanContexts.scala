package scalan

import scalan.primitives._
import scalan.staged.{Expressions, BaseExp}
import scalan.seq.BaseSeq
import scalan.arrays.{ArrayViewsSeq, ArrayViews, ArrayViewsExp}

//import ext.Options
//import lms.common._
//import lms.ppl._
//import sets.Sets

trait Scalan
  extends Base
//     with Pipes
     with Elems
//     with Descs
//     with Arrays
     with Views
//     with Chunks
//     with Sets
//     with Zipping
     with Tuples
     with TypeSum
     with ArrayOps
     with ArrayViews
//     with PArrayOps
     with NumericOps
//     with MathOps
     with LogicalOps
//     with OrderingOps
//     with FractionalOps
     with Functions
     with IfThenElse
     with Equal
     with Monoids
{
}

trait ScalanDsl
  extends Scalan
     with ProxyBase
{
}

trait ScalanSeq
  extends Scalan
  with BaseSeq
  with ElemsSeq
//  with DescsSeq
//  with SeqArrays
//  with SeqChunks
//  with SeqPipes
//  with SeqSets
  with ViewsSeq
//  with SeqArrayExtensions
  with TuplesSeq
  with TypeSumSeq
  with FunctionsSeq
  with IfThenElseSeq
  with ArrayOpsSeq
  with ArrayViewsSeq
//  with OrderingOpsSeq
  with NumericOpsSeq
  with EqualSeq
//  with MathOpsSeq
  with LogicalOpsSeq
//  with FractionalOpsSeq
{
  type Rep[+A] = A

//  lazy val parrayCanBeReified: CanBeReified[PArray] = new CanBeReified[PArray] {
//    def resolve[A](sym: PA[A]): PArray[A] = sym
//  }
//
//  lazy val pipeCanBeReified: CanBeReified[Pipe] = new CanBeReified[Pipe] {
//    def resolve[A](sym: P[A]): Pipe[A] = sym
//  }
//
//  lazy val chunksCanBeReified: CanBeReified[Chunks] = new CanBeReified[Chunks] {
//    def resolve[A](sym: Ch[A]): Chunks[A] = sym
//  }
//
//  implicit def reifyObject[A: Elem](obj: ReifiableObject[A]): Rep[A] = obj
//
//  def resolvePArray[A](s: PA[A]): PArray[A] = parrayCanBeReified.resolve(s)
//  def resolvePipe[A](s: P[A]): Pipe[A] = pipeCanBeReified.resolve(s)
//  def resolveChunks[A](s: Ch[A]): Chunks[A] = chunksCanBeReified.resolve(s)
//
//  // override implicit def liftElementValue[A:Elem](x: A): Rep[A] = element[A].toRep(x)
//
//  //  implicit def PairArrayExtensions[A, B]( ps: PArray[(A, B)]): PairArray[A, B] = ps.asInstanceOf[PairArray[A, B]]
//  //  implicit def SumArrayExtensions[A, B]( ps: PArray[(A|B)]): SumArray[A, B] = ps.asInstanceOf[SumArray[A, B]]
//  //  implicit def NestedArrayExtensions[A]( nested: NArray[A]): NestedArray[A] = nested.asInstanceOf[NestedArray[A]]


}

trait ScalanSeqImplementation
  extends ScalanDsl
  with ScalanSeq
  with ProxySeq
  //with StringOpsSeq
  //     with ExperimentalOpsSeq
{
  //implicit def liftElementValue[A:Elem](x: A): Rep[A] = x
}

trait ScalanStaged
  extends Scalan
  with BaseExp
  with TuplesExp
  with TypeSumExp
//  with StagedImplBase
  with ElemsExp
//  with DescsExp
//  with StagedArrays
//  with StagedArrayExtensions
//  with StagedPipes
//  with StagedChunks
//  with StagedSets
  with ViewsExp
  with ArrayOpsExp
  with ArrayViewsExp
//  with Transforming
  with NumericOpsExp
//  with MathOpsExp
  with LogicalOpsExp
//  with OrderingOpsExp
//  with FractionalOpsExp
  with FunctionsExp
  with IfThenElseExp
//  with Graphs
//  with Optimizations
{
  type Rep[+A] = Exp[A]

//  lazy val parrayCanBeReified: CanBeReified[PArray] = new CanBeReified[PArray] {
//
//    def resolve[A](sym: PA[A]): PArray[A] = sym match {
//      case Def(d: PADef[_]) => d.asInstanceOf[PArray[A]]
//      case s: Exp[_] => {
//        val paElem = s.Elem.asInstanceOf[PArrayElem[A]]
//        implicit val ea = paElem.ea
//        VarPA(sym)
//      }
//      case _ => ???("cannot resolve ReifiableObject for symbol:", sym)
//    }
//  }
//
//  lazy val pipeCanBeReified: CanBeReified[Pipe] = new CanBeReified[Pipe] {
//
//    def resolve[A](sym: P[A]): Pipe[A] = sym match {
//      case Def(d: PipeDef[_]) => d.asInstanceOf[Pipe[A]]
//      case s: Exp[_] => {
//        val pipeElem = s.Elem.asInstanceOf[PipeElem[A]]
//        implicit val ea = pipeElem.ea
//        VarPipe(sym)
//      }
//      case _ => ???("cannot resolve ReifiableObject for symbol:", sym)
//    }
//  }
//
//  lazy val chunksCanBeReified: CanBeReified[Chunks] = new CanBeReified[Chunks] {
//
//    def resolve[A](sym: Ch[A]): Chunks[A] = sym match {
//      case Def(d: ChunksDef[_]) => d.asInstanceOf[Chunks[A]]
//      case s: Exp[_] => {
//        val chunksElem = s.Elem.asInstanceOf[ChunksElem[A]]
//        implicit val ea = chunksElem.ea
//        VarChunks(sym)
//      }
//      case _ => ???("cannot resolve ReifiableObject for symbol:", sym)
//    }
//  }
//
//  implicit def reifyObject[A: Elem](obj: ReifiableObject[A]): Rep[A] = toExp(obj, fresh[A])
//
//  //override implicit def resolveObject[A, C[_]](s: Rep[C[A]])(implicit cbr: CanBeReified[C]): C[A] = cbr.resolve(s)
//  def resolvePArray[A](s: PA[A]): PArray[A] = parrayCanBeReified.resolve(s)
//  def resolvePipe[A](s: P[A]): Pipe[A] = pipeCanBeReified.resolve(s)
//  def resolveChunks[A](s: Ch[A]): Chunks[A] = chunksCanBeReified.resolve(s)
//
//  //override implicit def liftElementValue[A:Elem](x: A): Rep[A] = element[A].toRep(x)
//
//  //  implicit def PairArrayExtensions[A, B]( ps: PArray[(A, B)]): PairArray[A, B] = ps.asInstanceOf[PairArray[A, B]]
//  //  implicit def SumArrayExtensions[A, B]( ps: PArray[(A|B)]): SumArray[A, B] = ps.asInstanceOf[SumArray[A, B]]
//  //  implicit def NestedArrayExtensions[A]( nested: NArray[A]): NestedArray[A] = nested.asInstanceOf[NestedArray[A]]


}

//trait ScalanStagedImplementation extends ScalanDsl with ScalanStaged with EqualExp with ExperimentalOpsExp
trait ScalanStagedImplementation
  extends ScalanDsl
  with ScalanStaged
  with EqualExp
  with Expressions
{
  //implicit def liftElementValue[A:Elem](x: A): Rep[A] = element[A].toRep(x)
}

trait ScalanCtxSeq extends ScalanSeqImplementation {}
trait ScalanCtxStaged extends ScalanStagedImplementation {}