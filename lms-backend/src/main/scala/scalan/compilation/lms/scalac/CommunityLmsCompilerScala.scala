package scalan.compilation.lms.scalac

import java.io.File

import scala.collection.Seq
import scala.reflect.runtime.universe._
import scalan.ScalanCommunityDslExp
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.CommunityBridgeScala

trait CommunityLmsCompilerScala extends LmsCompilerScala with CommunityBridgeScala with ScalanCommunityDslExp { self: ScalanCommunityDslExp =>

  import scala.language.reflectiveCalls

  new ScalaMappingDSL with MappingTags {

    val scalan_collections_SSeq = {
      val sseqClass = findDefinition(SSeq) match {
        case Some(TableEntry(sym, rhs)) =>
          rhs.getClass
      }

      new ClassType(Symbol(sseqClass.getName)) {
        val apply = Method('apply, typeOf[Seq[_]], MethodArg(typeOf[Array[_]]))
        val single = Method('single, typeOf[Seq[_]])
        val empty = Method('empty, typeOf[Seq[_]])
      }
    }

    val scala_collection_Seq = new ScalaLib() {
      val arrayToList = ScalaFunc(Symbol("(new AnyRef {def apply[T](arr: Array[T]):List[T] = arr.toList})"))(false)
      val single = ScalaFunc(Symbol("scala.collection.Seq"))(false)
      val empty = ScalaFunc(Symbol("Seq.empty"))(false)
    }

    val mapping = new ScalaMapping {
      val functionMap = Map( scalan_collections_SSeq.apply -> scala_collection_Seq.arrayToList
                           , scalan_collections_SSeq.empty -> scala_collection_Seq.empty
                           , scalan_collections_SSeq.single -> scala_collection_Seq.single)
    }
  }

  override protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                                (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    super.doBuildExecutable[A, B](sourcesDir, executableDir, functionName, graph, graphVizConfig)(compilerConfig, eInput, eOutput)
  }

  override def newObj[A: Manifest](m: LmsMirror, aClass: Class[_], args: Seq[Rep[_]], newKeyWord: Boolean): lms.Exp[A] = {
    val name = mappedClassName(aClass) match {
      case Some(n) => n
      case _ => aClass.getName
    }
    lms.newObj[A](name, args.map(v => m.symMirrorUntyped(v)), newKeyWord)
  }
}
