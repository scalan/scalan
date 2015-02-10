package scalan.compilation.lms

import java.io.File

import scalan.ScalanCtxExp
import scalan.util.FileUtil

trait LmsBridge {

  val scalan: ScalanCtxExp
  val lms: LmsBackend

  type SymMirror = Map[scalan.Exp[_], lms.Exp[A] forSome {type A}]
  type FuncMirror = Map[scalan.Exp[_], (lms.Exp[A] => lms.Exp[B]) forSome {type A; type B}]
  type ExpMirror = Seq[lms.Exp[_]]

  type Mirror = (ExpMirror, SymMirror, FuncMirror)

  type EntryTransformer = PartialFunction[scalan.TableEntry[_], Mirror]
  type DefTransformer = PartialFunction[scalan.Def[_], Mirror]

  def defTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]): DefTransformer = {
    case x => scalan.!!!(s"ScalanLMSBridge: Don't know how to mirror symbol ${x.self.toStringWithDefinition}")
  }

  def tableTransformer(m: Mirror, g: scalan.AstGraph): EntryTransformer = {
    case e => defTransformer(m, g, e)(e.rhs)
  }

  def mirrorLambdaToLmsFunc[I, R](m: Mirror)(lam: scalan.Lambda[I, R]): (lms.Exp[I] => lms.Exp[R]) = {
    val (expMirror, symMirror, funcMirror) = m
    val lamX = lam.x
    val f = { x: lms.Exp[I] =>
      val sched = lam.scheduleSingleLevel
      val (lamExps, _, _) = mirrorDefs((expMirror, symMirror + ((lamX, x)), funcMirror))(lam, sched)
      val res = lamExps.lastOption.getOrElse(x)
      res.asInstanceOf[lms.Exp[R]]
    }
    f
  }

  /* Mirror block */
  def mirrorBlockToLms[R](m: Mirror)(block: scalan.ThunkDef[_], dflt: scalan.Rep[_]): () => lms.Exp[R] = { () =>
    val (_, symMirror, _) = m
    val sched = block.scheduleSingleLevel
    val (blockExps, _, _) = mirrorDefs(m)(block, sched)
    val res = blockExps.lastOption.getOrElse(symMirror(dflt))
    res.asInstanceOf[lms.Exp[R]]
  }

  def mirrorDefs(m: Mirror)(fromGraph: scalan.AstGraph, defs: Seq[scalan.TableEntry[_]]): Mirror = {
    val (_, symMirror, funcMirror) = m
    val init: Mirror = (List.empty[lms.Exp[_]], symMirror, funcMirror)
    val (lmsExps, finalSymMirr, finalFuncMirr) = defs.foldLeft(init)((m, t) => tableTransformer(m, fromGraph)(t))
    (lmsExps, finalSymMirr, finalFuncMirr)
  }

  // can't just return lmsFunc: lms.Exp[A] => lms.Exp[B], since mirrorDefs needs to be called in LMS context
  def apply[A, B](g: scalan.PGraph)(x: lms.Exp[A]) = {
    val emptyMirror: Mirror = (Seq.empty, Map.empty, Map.empty)
    val (_, _, finalFuncMirror) = mirrorDefs(emptyMirror)(g, g.schedule)
    val lmsFunc = finalFuncMirror(g.roots.last).asInstanceOf[lms.Exp[A] => lms.Exp[B]]
    lmsFunc(x)
  }

  def createManifest[T]: PartialFunction[scalan.Elem[T], Manifest[_]] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    case scalan.UnitElement => Manifest.Unit
    case scalan.BoolElement => Manifest.Boolean
    case scalan.ByteElement => Manifest.Byte
    case scalan.ShortElement => Manifest.Short
    case scalan.IntElement => Manifest.Int
    case scalan.CharElement => Manifest.Char
    case scalan.LongElement => Manifest.Long
    case scalan.FloatElement => Manifest.Float
    case scalan.DoubleElement => Manifest.Double
    case scalan.StringElement => manifest[String]
    case scalan.PairElem(eFst, eSnd) =>
      Manifest.classType(classOf[(_, _)], createManifest(eFst), createManifest(eSnd))
    case scalan.SumElem(eLeft, eRight) =>
      Manifest.classType(classOf[Either[_, _]], createManifest(eLeft), createManifest(eRight))
    case el: scalan.FuncElem[_, _] =>
      Manifest.classType(classOf[_ => _], createManifest(el.eDom), createManifest(el.eRange))
    case el: scalan.ArrayElem[_] =>
      Manifest.arrayType(createManifest(el.eItem))
    case el: scalan.ArrayBufferElem[_] =>
      Manifest.classType(classOf[scala.collection.mutable.ArrayBuilder[_]], createManifest(el.eItem))
    case el: scalan.ListElem[_] ⇒
      Manifest.classType(classOf[List[_]], createManifest(el.eItem))
    case el: scalan.MMapElem[_,_] =>
      Manifest.classType(classOf[java.util.HashMap[_,_]], createManifest(el.eKey), createManifest(el.eValue))
    case el => scalan.???(s"Don't know how to create manifest for $el")
  }

  def emitSource[A, B](sourcesDir: File, extension: String, functionName: String, graph: scalan.PGraph, eInput: scalan.Elem[A], eOutput: scalan.Elem[B]): File = {
    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val sourceFile = new File(sourcesDir, s"$functionName.$extension")
        FileUtil.withFile(sourceFile) { writer =>
          val codegen = lms.codegen
          val lmsFunc = apply[a, b](graph) _
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
