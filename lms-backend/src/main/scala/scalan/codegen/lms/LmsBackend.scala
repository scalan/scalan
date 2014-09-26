package scalan.codegen.lms

import java.io._
import java.net.URLClassLoader

import scalan.codegen.Backend
import scalan.codegen.GraphVizExport
import scalan.linalgebra.VectorsDslExp
import scalan.community.ScalanCommunityExp

trait LmsBridge[A,B] {
  val scalan: ScalanCommunityExp with LmsBackend with VectorsDslExp // TODO remove this!

  def outerApply[Ctx <: scalan.Transformer](app: LmsFunction[A,B])(in: app.Exp[A], g: scalan.ProgramGraph[Ctx]):app.Exp[B]

  class LmsFacade[Ctx <: scalan.Transformer](g: scalan.ProgramGraph[Ctx]) {
    val lFunc = new LmsFunction[A,B] {
      def apply(in: this.Exp[A]): this.Exp[B] = {
        outerApply(this)(in, g)
      }
    }
  }

  def getFacade[Ctx <: scalan.Transformer](g: scalan.ProgramGraph[Ctx]) = new LmsFacade[Ctx](g)
}

trait MyBridge[A,B] extends LmsBridge[A,B] {

  def outerApply[Ctx <: scalan.Transformer](lFunc: LmsFunction[A,B])(in: lFunc.Exp[A], g: scalan.ProgramGraph[Ctx]): lFunc.Exp[B] = {

    import scala.collection.Map

    val emptySymMirror = Map.empty[scalan.Exp[_], lFunc.Exp[X] forSome {type X}]
    val emptyFuncMirror = Map.empty[scalan.Exp[_], ( lFunc.Exp[X] => lFunc.Exp[Y]) forSome {type X; type Y}]

    val definitions = g.schedule

    type SymMirror = Map[scalan.Exp[_], lFunc.Exp[A] forSome {type A}]
    
    type FuncMirror = Map[scalan.Exp[_],( lFunc.Exp[A] => lFunc.Exp[B]) forSome { type A; type B } ]

    def mirrorLambdaToLmsFunc[I, R](lam:scalan.Lambda[I,R],
                                    symMirror: SymMirror,
                                    funcMirror: FuncMirror) : (lFunc.Exp[I] => lFunc.Exp[R]) = {
      val lamX = lam.x
      val f = { x: lFunc.Exp[I] =>
        val sched = lam.schedule
        val (lamExps, _, _) = mirrorDefs(sched, symMirror + ((lamX, x)), funcMirror )
        val res = if (lamExps.isEmpty) x else (lamExps.last)
        res.asInstanceOf[lFunc.Exp[R]]
      }
      f
    }

    /* Mirror block */
    def mirrorDefs(defs: Seq[scalan.TableEntry[_]], symMirror: SymMirror, funcMirror: FuncMirror):
      (Seq[lFunc.Exp[_]], SymMirror, FuncMirror) =
    {
      val (lmsExps, finalSymMirr, finalFuncMirr) = defs.foldLeft(List.empty[lFunc.Exp[_]], symMirror, funcMirror) {
        case ((exps, symMirr, funcMirr),tp) => {
          val s = tp.sym
          tp.rhs match {
            case lam:scalan.Lambda[a, b] => {
              val f = mirrorLambdaToLmsFunc[a,b](lam, symMirr, funcMirr)
              (exps, symMirr, funcMirr + ((s, f)) )
            }
            case c@scalan.Const(_) => {
              val exp = lFunc.unitD(c.x)
              (exps ++ List(exp), symMirr + ((s, exp)), funcMirr )
            }
            case scalan.First(tuple) => {
              tuple.elem match {
                case pe: scalan.PairElem[_, _] =>
                  (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd))  match {
                    case (mA:Manifest[a], mB: Manifest[b]) =>
                      val tup = symMirr(tuple).asInstanceOf[lFunc.Exp[(a,b)]]
                      val exp = lFunc.first[a,b](tup)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }
            case scalan.Second(tuple) => {
              tuple.elem match {
                case pe: scalan.PairElem[_, _] =>
                  (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd))  match {
                    case (mA:Manifest[a], mB: Manifest[b]) =>
                      val tup = symMirr(tuple).asInstanceOf[lFunc.Exp[(a,b)]]
                      val exp = lFunc.second[a,b](tup)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }
            case scalan.NumericTimes(arg1, arg2, n) => {
              scalan.createManifest(arg1.elem) match {
                case (mA:Manifest[a]) =>
                  val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[a]]
                  val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[a]]
                  val n1 = n.asInstanceOf[Numeric[a]]
                  val exp = lFunc.opMult(arg1_, arg2_)(n1, mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case scalan.NumericPlus(arg1, arg2, n) => {
              scalan.createManifest(arg1.elem) match {
                case (mA:Manifest[a]) =>
                  val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[a]]
                  val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[a]]
                  val n1 = n.asInstanceOf[Numeric[a]]
                  val exp = lFunc.opPlus(arg1_, arg2_)(n1, mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case scalan.NumericDivInt(arg1, arg2) =>
              val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[Int]]
              val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[Int]]
              val exp = lFunc.opDiv(arg1_, arg2_)(implicitly[Numeric[Int]], manifest[Int])
	          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr)
            case scalan.NumericModInt(arg1, arg2) =>
              val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[Int]]
              val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[Int]]
              val exp = lFunc.opMod(arg1_, arg2_)
	          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr)
            case scalan.NumericDiv(arg1, arg2, n) => {
              scalan.createManifest(arg1.elem) match {
                case (mA:Manifest[a]) =>
                  val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[a]]
                  val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[a]]
                  val n1 = n.asInstanceOf[Numeric[a]]
                  val exp = lFunc.opDiv(arg1_, arg2_)(n1, mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case scalan.NotEqual(arg1, arg2) => {
              scalan.createManifest(arg1.elem) match {
                case (mA:Manifest[a]) =>
                  val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[a]]
                  val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[a]]
                  val exp = lFunc.opNeq[a](arg1_, arg2_)(mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case apply@scalan.ArrayApply(xs, ind) => {
              scalan.createManifest(apply.selfType) match {
                case (mA:Manifest[a]) =>
                  val xs_ = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                  val ind_ = symMirr(ind).asInstanceOf[lFunc.Exp[Int]]
                  val exp = lFunc.arrayGet[a](xs_, ind_)(mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case scalan.ArrayApplyMany(xs, idxs) => {
              (xs.elem) match {
                case (el: scalan.ArrayElem[_]) =>
                  scalan.createManifest(el.eItem) match {
                  case (mA:Manifest[a]) =>
                    val xs_ = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                    val idxs_ = symMirr(idxs).asInstanceOf[lFunc.Exp[Array[Int]]]
                    val exp = lFunc.arrayGather[a](xs_, idxs_)(mA)
                    (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                }
              }
            }
            case scalan.ArrayLength(xs) => {
              scalan.createManifest(xs.elem) match {
                case (mA:Manifest[a]) =>
                  val xs_ = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                  val exp = lFunc.arrayLength[a](xs_)(mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case scalan.ArrayRangeFrom0(n) => {
              val n_ = symMirr(n).asInstanceOf[lFunc.Exp[Int]]
              val exp = lFunc.indexRangeD(n_)
              (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
            }
            case scalan.ArrayZip(arg1, arg2) => {
              (arg1.elem,arg2.elem) match {
                case (el1: scalan.ArrayElem[_], el2: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el1.eItem), scalan.createManifest(el2.eItem)) match {
                    case (mA:Manifest[a], mB:Manifest[b]) =>
                      val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[Array[a]]]
                      val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[Array[b]]]
                      val exp = lFunc.opZip[a,b](arg1_, arg2_)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }
            case map@scalan.ArrayMap(source, lambdaSym@scalan.Def(lam:scalan.Lambda[_,_]) ) => {
              (source.elem,map.selfType) match {
                case (el: scalan.ArrayElem[_], el1: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el.eItem), scalan.createManifest(el1.eItem)) match {
                    case (mA:Manifest[a], mB: Manifest[b]) =>
                      val f = mirrorLambdaToLmsFunc[a,b](lam.asInstanceOf[scalan.Lambda[a, b]], symMirr, funcMirr)//(mA, mB)
                      val lmsSource = symMirr(source).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.mapArray[a,b](lmsSource, f)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr + ((lambdaSym,f)))
                  }
              }
            }
            case filter@scalan.ArrayFilter(source, lambdaSym@scalan.Def(lam:scalan.Lambda[_,_]) ) => {
              (filter.selfType) match {
                case (el: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el.eItem)) match {
                    case (mA:Manifest[a]) =>
                      val f = mirrorLambdaToLmsFunc[a,Boolean](lam.asInstanceOf[scalan.Lambda[a, Boolean]], symMirr, funcMirr)//(mA, mB)
                      val lmsSource = symMirr(source).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.filterArray[a](lmsSource, f)(mA)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr + ((lambdaSym,f)))
                  }
              }
            }
            /* This is reduce */
            case scalan.ArrayReduce(source, monoid) => {
              (monoid, source.elem) match {
                case (monoid, el: scalan.ArrayElem[_]) if monoid.opName == "+" => {
                  scalan.createManifest(el.eItem) match {
                    case (mA: Manifest[a]) =>
                      val lmsSource = symMirr(source).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.reduce[a](lmsSource)(mA)
                      (exps ++ List(exp), symMirr + ((s, exp)), funcMirr)
                  }
                }
                case _ => scalan.!!!("ScalanLMSBridge: Unfortunately, only Plus monoid is supported by lms")
              }
            }
            case scalan.ArrayStride(xs, start, length, stride) =>
              xs.elem match {
                case el: scalan.ArrayElem[a] =>
                  val mA = scalan.createManifest(el.eItem).asInstanceOf[Manifest[a]]
                  val lmsXs = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                  val lmsStart = symMirr(start).asInstanceOf[lFunc.Exp[Int]]
                  val lmsLength = symMirr(length).asInstanceOf[lFunc.Exp[Int]]
                  val lmsStride = symMirr(stride).asInstanceOf[lFunc.Exp[Int]]
                  val exp = lFunc.strideArray(lmsXs, lmsStart, lmsLength, lmsStride)(mA)
                  (exps ++ List(exp), symMirr + ((s, exp)), funcMirr)
              }
            case scalan.DotSparse(i1, v1, i2, v2) => {
              (v1.elem) match {
                case (el: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el.eItem)) match {
                    case (mA:Manifest[a]) =>
                      val i1_ = symMirr(i1).asInstanceOf[lFunc.Exp[Array[Int]]]
                      val i2_ = symMirr(i2).asInstanceOf[lFunc.Exp[Array[Int]]]
                      val v1_ = symMirr(v1).asInstanceOf[lFunc.Exp[Array[a]]]
                      val v2_ = symMirr(v2).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.opDotProductSV[a](i1_, v1_, i2_, v2_)(mA)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }

            case _ => scalan.!!!(s"ScalanLMSBridge: Don't know how to mirror symbol ${s.toStringWithDefinition}")
          }
        }
      }
      (lmsExps, finalSymMirr, finalFuncMirr)
    }

    val (lmsExps, finalSymMirror, finalFuncMirror) = mirrorDefs(definitions, emptySymMirror, emptyFuncMirror)
    val res = finalFuncMirror(g.roots.last).asInstanceOf[lFunc.Exp[A] => lFunc.Exp[B]](in)

    res
  }
}

trait LmsBackend extends Backend { self: ScalanCommunityExp with GraphVizExport with VectorsDslExp =>

  case class Config(extraCompilerOptions: Seq[String])

  implicit val defaultConfig = Config(Seq.empty)

  def makeBridge[A, B]: MyBridge[A, B] = 
    new MyBridge[A, B] {
      val scalan: LmsBackend.this.type = self
    }

  protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                                      (config: Config, eInput: Elem[A], eOutput: Elem[B]) = {
    if (emitGraphs) {
      val dotFile = new File(sourcesDir, functionName + ".dot")
      this.emitDepGraph(func, dotFile, false)
    }

    val g0 = new PGraph(List(func))

    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".scala")

    func.elem match {
      case el:FuncElem[_,_] =>
        (createManifest(el.eDom), createManifest(el.eRange)) match {
          case (mA:Manifest[a], mB:Manifest[b]) =>
            val bridge = makeBridge[a, b]
            val facade = bridge.getFacade(g0.asInstanceOf[bridge.scalan.PGraph])
            val codegen = facade.lFunc.codegen

            val writer = new PrintWriter(new FileOutputStream(outputSource.getAbsolutePath))
            try {
              codegen.emitSource[a, b](facade.lFunc.apply, functionName, writer)(mA, mB)
              codegen.emitDataStructures(writer)
            } finally {
              writer.close()
            }
        }
    }

    val command = Seq("scalac", "-d", jarPath(functionName, executableDir)) ++ config.extraCompilerOptions :+
      outputSource.getAbsolutePath

    launchProcess(sourcesDir, command: _*)
  }

  protected def doExecute[A, B](executableDir: File, functionName: String, input: A)
                               (config: Config, eInput: Elem[A], eOutput: Elem[B]): B = {
    val url = new File(jarPath(functionName, executableDir)).toURI.toURL
    val classLoader = new URLClassLoader(scala.Array(url))
    val cls = classLoader.loadClass(functionName)
    val argumentClass = eInput.classTag.runtimeClass
    val method = cls.getMethod("apply", argumentClass)
    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }

  private def jarPath(functionName: String, executableDir: File) =
    s"${executableDir.getAbsolutePath}/$functionName.jar"

  def createManifest[T](eA: Elem[T]): Manifest[_] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    val manifest = eA match {
      case el: BaseElem[_] =>
        el.tag.tpe.toString()  match {
          case "Double" => Manifest.Double
          case "Int" => Manifest.Int
          case "Unit" => Manifest.Unit
          case tpe => ???(s"Don't know how to create manifest for base type $tpe")
        }
      case el: PairElem[_, _] =>
        Manifest.classType(classOf[(_, _)], createManifest(el.eFst), createManifest(el.eSnd))
      case el: ArrayElem[_] => {
        Manifest.arrayType(createManifest(el.eItem))
      }
      case el: FuncElem[_,_] => {
        Manifest.classType(classOf[_ => _], createManifest(el.eDom), createManifest(el.eRange))
      }
      case el => ???(s"Don't know how to create manifest for $el")
    }
    manifest
  }
}
