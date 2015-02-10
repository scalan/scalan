package scalan
package compilation
package lms

import java.io.File

import scalan.util.FileUtil

trait LmsCompiler extends Compiler { self: ScalanCtxExp =>

  def makeBridge: LmsBridge

  def createManifest[T]: PartialFunction[Elem[T], Manifest[_]] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    case UnitElement => Manifest.Unit
    case BoolElement => Manifest.Boolean
    case ByteElement => Manifest.Byte
    case ShortElement => Manifest.Short
    case IntElement => Manifest.Int
    case CharElement => Manifest.Char
    case LongElement => Manifest.Long
    case FloatElement => Manifest.Float
    case DoubleElement => Manifest.Double
    case StringElement => manifest[String]
    case PairElem(eFst, eSnd) =>
      Manifest.classType(classOf[(_, _)], createManifest(eFst), createManifest(eSnd))
    case SumElem(eLeft, eRight) =>
      Manifest.classType(classOf[Either[_, _]], createManifest(eLeft), createManifest(eRight))
    case el: FuncElem[_, _] =>
      Manifest.classType(classOf[_ => _], createManifest(el.eDom), createManifest(el.eRange))
    case el: ArrayElem[_] =>
      Manifest.arrayType(createManifest(el.eItem))
    case el: ArrayBufferElem[_] =>
      Manifest.classType(classOf[scala.collection.mutable.ArrayBuilder[_]], createManifest(el.eItem))
    case el: ListElem[_] ⇒
      Manifest.classType(classOf[List[_]], createManifest(el.eItem))
    case el: MMapElem[_,_] =>
      Manifest.classType(classOf[java.util.HashMap[_,_]], createManifest(el.eKey), createManifest(el.eValue))
    case el => ???(s"Don't know how to create manifest for $el")
  }

  def emitSource[A, B](sourcesDir: File, extension: String, functionName: String, graph: PGraph, eInput: Elem[A], eOutput: Elem[B]): File = {
    val bridge = makeBridge
    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val sourceFile = new File(sourcesDir, s"$functionName.$extension")
        FileUtil.withFile(sourceFile) { writer =>
          val codegen = bridge.lms.codegen
          val lmsFunc = bridge.apply[a, b](graph.asInstanceOf[bridge.scalan.PGraph]) _
          codegen.emitSource[a, b](lmsFunc, functionName, writer)(mA, mB)
          //          val s = bridge.lms.fresh[a](mA)
          //          val body = codegen.reifyBlock(facade.apply(s))(mB)
          //          codegen.emitSource(List(s), body, functionName, writer)(mB)
          //          val bridge.lms.TP(sym,_) = bridge.lms.globalDefs.last
          //          codegen.emitDepGraph( sym, new File( sourcesDir, functionName + "-LMS.dot" ).getAbsolutePath )
          codegen.emitDataStructures(writer)
        }
        sourceFile
    }
  }
}
