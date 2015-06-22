package scalan.compilation.lms.uni

import scalan.{ScalanCtxExp, Metadata}

/**
 * Created by adel on 6/4/15.
 */

class SymbolsMarkerForSelectCodegen[ScalanCake <: ScalanCtxExp](scalan: ScalanCake) {
  val ScalanIR: ScalanCake = scalan
  import ScalanIR._

  type CompilationPipelineContext = Unit
  val defaultCompilationPipelineContext: CompilationPipelineContext = ()

  def mark[A, B](func: Exp[A => B], context: CompilationPipelineContext, config: NativeMethodsConfig):List[Exp[_]] = {
    config.rootIsNative match {
      case true =>
        func.setMetadata(codegenChangeKey)(KnownCodegens.pairToString(KnownCodegens.Scala, KnownCodegens.Cxx))
        List(func)
      case _  => {
        val res = for(sym <- syms(func)) yield {
          println("mark: " + sym)
          val sub = findDefinition(sym) match {
            case Some(t: TableEntry[_]) if t.isLambda => for(s <- syms(t.rhs)) yield { markSubgraph(s, KnownCodegens.Scala, context, config) }
            case _ => !!!("SymbolsMarkerForSelectCodegen.mark ERROR: lambda in TableEntry expected")
          }
          sub.flatten
        }
        res.flatten
      }
    }

  }

  protected def markSubgraph[T](exp: Exp[T], from: KnownCodegens.CodegenType, context: CompilationPipelineContext, config: NativeMethodsConfig): List[Exp[_]] = {
    for(sym <- syms(exp)) {
      //println("subMark: " + sym)
      findDefinition(sym) match {
        case Some(t: TableEntry[_]) if t.isLambda => {
          //println("   subMark for t as lambda: " + t)
          for (s <- syms(t.rhs)) {
            markSubgraph(s, from, context, config)
          }
        }
        case Some(t: TableEntry[_]) if !t.isLambda => {
          //println("   subMark for t as not lambda: " + t)
          for (s <- syms(t.rhs)) {
            markSubgraph(s, from, context, config)
          }
        }
        case x => //println("   subMark for ???: " + x)
      }
    }
    Nil //todo - return set of lambdas, marked for using some codegen
  }

  lazy val isNativeKey = MetaKey[Boolean]("isNative")
  lazy val codegenChangeKey = MetaKey[String]("codegenChangeKey")
  lazy val codegenKey = MetaKey[String]("codegenKey")
  //val isNivokeDisabled = MetaKey[Boolean]("isNivokeDisabled")


}
