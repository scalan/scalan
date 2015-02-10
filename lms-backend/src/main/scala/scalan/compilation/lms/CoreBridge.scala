package scalan
package compilation.lms

import java.util.HashMap
import scalan.compilation.language.Interpreter
import scalan.compilation.lms.scalac.LmsCompilerScala

trait CoreBridge[A, B] extends LmsBridge[A, B] with Interpreter with CommunityMethodMapping {

  // `LmsCompiler` mixed just to provide `createManifest` function
  val scalan: ScalanCtxExp with LmsCompiler
  val lms: CoreLmsBackendBase

  override def defTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]) =
    coreDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)

  def coreDefTransformer[T](m: Mirror, fromGraph: scalan.AstGraph, tp: scalan.TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = tp.sym

    val tt: DefTransformer = {
      case _: scalan.CompanionBase[_] =>
        // ignore companion objects
        m

      case lam: scalan.Lambda[a, b] =>
        val mA = scalan.createManifest(lam.x.elem).asInstanceOf[Manifest[a]]
        val mB = scalan.createManifest(lam.y.elem).asInstanceOf[Manifest[b]]
        val f = mirrorLambdaToLmsFunc[a, b](m)(lam)
        val fun = lms.fun(f)(mA, mB)
        (exps, symMirr + ((sym, fun)), funcMirr + ((sym, f)))

      case scalan.MethodCall(receiver, method, args, _) =>
        import lms.EffectId._
        val obj = symMirr(receiver.asInstanceOf[scalan.Exp[_]])

        val exp = getFunc(method) match {
          case Some(conf: LanguageConf#Fun) => conf.lib match {
            case e: ScalaLanguage#ScalaLib =>
//              if (!e.jar.isEmpty) scalan.extensionsJars += e.jar
              Manifest.classType(method.getDeclaringClass) match {
                case (mA: Manifest[a]) => lms.scalaMethod[a](null, PURE, e.pack + "." + conf.funcName.name, List(obj) ++ args.map(v => symMirr(v.asInstanceOf[scalan.Exp[_]])): _*)(mA)
              }
            case e: ScalaLanguage#EmbeddedObject if e.name == "lms" =>
              val name = conf.funcName.name
              import scala.reflect.runtime.universe._
              val m = runtimeMirror(obj.getClass.getClassLoader).reflect(lms)
              val lmsMethod = m.symbol.typeSignature.member(newTermName(name))
              m.reflectMethod(lmsMethod.asMethod).apply(obj, scalan.createManifest(receiver.asInstanceOf[scalan.Exp[_]].elem)).asInstanceOf[lms.Exp[_]]
          }
          case None =>
            Manifest.classType(method.getDeclaringClass) match {
              case (mA: Manifest[a]) => lms.scalaMethod[a](obj, PURE, method.getName, args
                /* filter out implicit ClassTag params */ .filter(_.isInstanceOf[scalan.Exp[_]])
                .map(v => symMirr(v.asInstanceOf[scalan.Exp[_]])): _*)(mA)
            }
        }
        (exps ++ scala.List(exp), symMirr + ((sym, exp)), funcMirr)

      case lr@scalan.NewObject(aClass, args, _) =>
        Manifest.classType(aClass) match {
          case (mA: Manifest[a]) =>
            val exp = lms.newObj[a](aClass.getCanonicalName, args.map(v => symMirr(v.asInstanceOf[scalan.Exp[_]])))(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.Apply(f, x) =>
        import scalan.FuncElemExtensions
        (scalan.createManifest(f.elem.eDom), scalan.createManifest(f.elem.eRange)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val fun = symMirr(f).asInstanceOf[lms.Exp[a => b]]
            val arg = symMirr(x).asInstanceOf[lms.Exp[a]]
            val exp = lms.doApply[a, b](fun, arg)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case c@scalan.Const(_) =>
        scalan.createManifest(c.selfType) match {
          case mA: Manifest[a] =>
            val x = c.x.asInstanceOf[a]
            val exp = lms.unitD(x)(mA)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case d@scalan.Left(l) =>
        import scalan.SumElemExtensions
        (scalan.createManifest(d.selfType.eLeft), scalan.createManifest(d.selfType.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val left = symMirr(l).asInstanceOf[lms.Exp[a]]
            val exp = lms.make_left[a, b](left)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case d@scalan.Right(r) =>
        import scalan.SumElemExtensions
        (scalan.createManifest(d.selfType.eLeft), scalan.createManifest(d.selfType.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val right = symMirr(r).asInstanceOf[lms.Exp[b]]
            val exp = lms.make_right[a, b](right)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.IsLeft(s) =>
        import scalan.SumElemExtensions
        (scalan.createManifest(s.elem.eLeft), scalan.createManifest(s.elem.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val sum = symMirr(s).asInstanceOf[lms.Exp[Either[a, b]]]
            val exp = lms.make_isLeft(sum)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.IsRight(s) =>
        import scalan.SumElemExtensions
        (scalan.createManifest(s.elem.eLeft), scalan.createManifest(s.elem.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val sum = symMirr(s).asInstanceOf[lms.Exp[Either[a, b]]]
            val exp = lms.make_isRight(sum)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.SumFold(s, l, r) =>
        import scalan.SumElemExtensions
        import scalan.FuncElemExtensions
        (scalan.createManifest(s.elem.eLeft), scalan.createManifest(s.elem.eRight), scalan.createManifest(l.elem.eRange)) match {
          case (mA: Manifest[a], mB: Manifest[b], mR: Manifest[r]) =>
            implicit val (imA, imB, imR) = (mA, mB, mR)
            val sum = symMirr(s).asInstanceOf[lms.Exp[Either[a, b]]]
            val left = symMirr(l).asInstanceOf[lms.Exp[a => r]]
            val right = symMirr(r).asInstanceOf[lms.Exp[b => r]]
            val exp = lms.make_fold(sum, left, right)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.Tup(fst, snd) =>
        (scalan.createManifest(fst.elem), scalan.createManifest(snd.elem)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            val first = symMirr(fst).asInstanceOf[lms.Exp[a]]
            val second = symMirr(snd).asInstanceOf[lms.Exp[b]]
            val exp = lms.tuple[a, b](first, second)(mA, mB)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.First(tuple) =>
        tuple.elem match {
          case pe: scalan.PairElem[_, _] =>
            (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val tup = symMirr(tuple).asInstanceOf[lms.Exp[(a, b)]]
                val exp = lms.first[a, b](tup)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case scalan.Second(tuple) =>
        tuple.elem match {
          case pe: scalan.PairElem[_, _] =>
            (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val tup = symMirr(tuple).asInstanceOf[lms.Exp[(a, b)]]
                val exp = lms.second[a, b](tup)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case scalan.ApplyUnOp(op, arg1) => {
        scalan.createManifest(arg1.elem) match {
          case (mA: Manifest[a]) => {
            val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[a]]
            val exp: lms.Exp[_] = op.asInstanceOf[scalan.UnOp[a, _]] match {
              case scalan.Not => lms.Not(arg1_.asInstanceOf[lms.Exp[Boolean]])
              case scalan.NumericNegate(n) => lms.Neg(arg1_)(mA, n.asInstanceOf[Numeric[a]])
              case scalan.NumericToDouble(n) =>
                mA match {
                  case Manifest.Int => lms.IntToDouble(arg1_.asInstanceOf[lms.Exp[Int]])
                  case Manifest.Float => lms.FloatToDouble(arg1_.asInstanceOf[lms.Exp[Float]])
                  case Manifest.Double => arg1_
                }
              case scalan.NumericToFloat(n) =>
                mA match {
                  case Manifest.Int => lms.IntToFloat(arg1_.asInstanceOf[lms.Exp[Int]])
                  case Manifest.Double => lms.DoubleToFloat(arg1_.asInstanceOf[lms.Exp[Double]])
                  case Manifest.Float => arg1_
                }
              case scalan.NumericToInt(n) =>
                mA match {
                  case Manifest.Float => lms.FloatToInt(arg1_.asInstanceOf[lms.Exp[Float]])
                  case Manifest.Double => lms.DoubleToInt(arg1_.asInstanceOf[lms.Exp[Double]])
                  case Manifest.Int => arg1_
                }
              case scalan.NumericToString() => lms.ToString(arg1_)
              case scalan.HashCode() => lms.hashCode(arg1_)
              case scalan.StringToInt() => lms.stringToInt(arg1_.asInstanceOf[lms.Exp[String]])
              case scalan.StringToDouble() => lms.stringToDouble(arg1_.asInstanceOf[lms.Exp[String]])
              case _ =>
                op.opName match {
                  case "Sin" => lms.Sin(arg1_.asInstanceOf[lms.Exp[Double]])
                  case "ToDouble" => lms.intToDouble(arg1_.asInstanceOf[lms.Exp[Int]])
            }
            }
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }
      case scalan.StringSubstr(str, start, end) => {
        val str_ = symMirr(str).asInstanceOf[lms.Exp[String]]
        val start_ = symMirr(start).asInstanceOf[lms.Exp[Int]]
        val end_ = symMirr(end).asInstanceOf[lms.Exp[Int]]
        val exp = lms.substring(str_, start_, end_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      }
      case scalan.StringApply(str, index) => {
        val str_ = symMirr(str).asInstanceOf[lms.Exp[String]]
        val index_ = symMirr(index).asInstanceOf[lms.Exp[Int]]
        val exp = lms.charAt(str_, index_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      }
      case scalan.ApplyBinOp(op, arg1, arg2) =>
        scalan.createManifest(arg1.elem) match {
          case (mA: Manifest[a]) =>
            val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[a]]
            val arg2_ = symMirr(arg2).asInstanceOf[lms.Exp[a]]
            val exp = op.asInstanceOf[scalan.BinOp[a, _]] match {
              case scalan.NumericTimes(n) =>
                lms.opMult(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.NumericPlus(n) =>
                lms.opPlus(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.NumericMinus(n) =>
                lms.opMinus(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.IntegralDivide(n) =>
                lms.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.IntegralMod(n) =>
                if (mA == Manifest.Int)
                  lms.opMod(arg1_.asInstanceOf[lms.Exp[Int]], arg2_.asInstanceOf[lms.Exp[Int]])
                else
                  throw new IllegalStateException(s"LMS only supports mod operation for Int, got $mA instead")
              case scalan.FractionalDivide(n) =>
                lms.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.Equals() =>
                lms.opEq[a](arg1_, arg2_)(mA)
              case scalan.OrderingLT(ord) =>
                lms.LT[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingLTEQ(ord) =>
                lms.LTEQ[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingGT(ord) =>
                lms.GT[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingGTEQ(ord) =>
                lms.GTEQ[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingMax(ord) =>
                lms.Max[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingMin(ord) =>
                lms.Min[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.And =>
                lms.And(arg1_.asInstanceOf[lms.Exp[Boolean]], arg2_.asInstanceOf[lms.Exp[Boolean]])
              case scalan.Or =>
                lms.Or(arg1_.asInstanceOf[lms.Exp[Boolean]], arg2_.asInstanceOf[lms.Exp[Boolean]])
              case scalan.StringConcat() =>
                lms.stringConcat(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case scalan.StringContains() =>
                lms.stringContains(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case scalan.StringStartsWith() =>
                lms.stringStartsWith(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case scalan.StringEndsWith() =>
                lms.stringEndsWith(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case scalan.StringMatches() =>
                lms.stringMatches(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case scalan.MathPow =>
                lms.Pow(arg1_.asInstanceOf[lms.Exp[Double]], arg2_.asInstanceOf[lms.Exp[Double]])
            }
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ThrowException(msg) => {
        val msg_ = symMirr(msg).asInstanceOf[lms.Exp[String]]
        val exp = lms.throwException(msg_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      }
      case scalan.Semicolon(left, right) => {
        (scalan.createManifest(left.elem), scalan.createManifest(right.elem)) match {
          case (mA: Manifest[a], mB: Manifest[b]) => {
            val left_ = symMirr(left).asInstanceOf[lms.Exp[a]]
            val right_ = symMirr(right).asInstanceOf[lms.Exp[b]]
            val exp = lms.block(left_, right_)(mA, mB)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }
      case i@scalan.IfThenElse(cond, iftrue, iffalse) =>
        scalan.createManifest(i.selfType) match {
          case (mA: Manifest[a]) =>
            val cond_ = symMirr(cond).asInstanceOf[lms.Exp[Boolean]]

            fromGraph.branches.ifBranches.get(sym) match {
              case Some(branches) =>
                def thenBody = mirrorBlockToLms(m)(branches.thenBody, iftrue)
                def elseBody = mirrorBlockToLms(m)(branches.elseBody, iffalse)
                val exp = lms.ifThenElse(cond_, thenBody, elseBody)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              case _ =>
                val then_ = symMirr(iftrue).asInstanceOf[lms.Exp[a]]
                val else_ = symMirr(iffalse).asInstanceOf[lms.Exp[a]]
                val exp = lms.ifThenElse(cond_, () => then_, () => else_)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      //
      // Array Buffer
      //
      case buf@scalan.ArrayBufferEmpty() => {
        buf.selfType match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val exp = lms.emptyArrayBuffer[t]()(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case buf@scalan.MakeArrayBuffer(ctx) => {
        buf.selfType match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val exp = lms.emptyArrayBuffer[t]()(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case buf@scalan.ArrayBufferFromElem(e) => {
        buf.selfType match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val v = symMirr(e).asInstanceOf[lms.Exp[t]]
                val exp = lms.arrayBufferFromElem(v)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferApply(buf, i) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val i_ = symMirr(i).asInstanceOf[lms.Exp[Int]]
                val exp = lms.arrayBufferApply(b_, i_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferLength(buf) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val exp = lms.arrayBufferLength(b_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case res@scalan.ArrayBufferMap(buf, lambdaSym@scalan.Def(lambda: scalan.Lambda[_, _])) => {
        (res.selfType, buf.elem) match {
          case (from: scalan.ArrayBufferElem[a], to: scalan.ArrayBufferElem[b]) => {
            (scalan.createManifest(from.eItem), scalan.createManifest(to.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[a]]]
                val f_ = mirrorLambdaToLmsFunc[a, b](m)(lambda.asInstanceOf[scalan.Lambda[a, b]])
                val exp = lms.arrayBufferMap(b_, f_)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f_)))
              }
            }
          }
        }
      }
      case scalan.ArrayBufferUpdate(buf, i, v) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val i_ = symMirr(i).asInstanceOf[lms.Exp[Int]]
                val v_ = symMirr(v).asInstanceOf[lms.Exp[t]]
                val exp = lms.arrayBufferUpdate(b_, i_, v_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferInsert(buf, i, v) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val i_ = symMirr(i).asInstanceOf[lms.Exp[Int]]
                val v_ = symMirr(v).asInstanceOf[lms.Exp[t]]
                val exp = lms.arrayBufferInsert(b_, i_, v_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferAppend(buf, v) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val v_ = symMirr(v).asInstanceOf[lms.Exp[t]]
                val exp = lms.arrayBufferAppend(b_, v_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferAppendArray(buf, a) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val a_ = symMirr(a).asInstanceOf[lms.Exp[Array[t]]]
                val exp = lms.arrayBufferAppendArray(b_, a_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferRemove(buf, i, n) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val i_ = symMirr(i).asInstanceOf[lms.Exp[Int]]
                val n_ = symMirr(n).asInstanceOf[lms.Exp[Int]]
                val exp = lms.arrayBufferRemove(b_, i_, n_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferReset(buf) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val exp = lms.arrayBufferReset(b_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.ArrayBufferToArray(buf) => {
        buf.elem match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val exp = lms.arrayBufferToArray(b_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case ab@scalan.ArrayBufferRep(buf) => {
        ab.selfType match {
          case elem: scalan.ArrayBufferElem[t] => {
            val exp = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }
      case buf@scalan.ArrayBufferUsingFunc(count, lambdaSym@scalan.Def(lambda: scalan.Lambda[_, _])) => {
        buf.selfType match {
          case elem: scalan.ArrayBufferElem[t] => {
            scalan.createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val n = symMirr(count).asInstanceOf[lms.Exp[Int]]
                val f = mirrorLambdaToLmsFunc[Int, t](m)(lambda.asInstanceOf[scalan.Lambda[Int, t]])
                val exp = lms.arrayBufferUsingFunc(n, f)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
              }
            }
          }
        }
      }


      //
      // Map
      //
      case scalan.AppendMultiMap(map, key, value) => {
        (key.elem, value.elem) match {
          case (eK: scalan.Elem[k], eV: scalan.Elem[v]) => {
            (scalan.createManifest(eK), scalan.createManifest(eV)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, scala.collection.mutable.ArrayBuilder[v]]]]
                val key_ = symMirr(key).asInstanceOf[lms.Exp[k]]
                val value_ = symMirr(value).asInstanceOf[lms.Exp[v]]
                val exp = lms.multiMapAppend(map_, key_, value_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)

              }
            }
          }
        }
      }
      case pm@scalan.VarMM(map) => {
        pm.selfType match {
          case elem: scalan.MMapElem[k, v] => {
            val exp = symMirr(map).asInstanceOf[lms.Exp[scalan.MMap[k, v]]]
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }

      case map@scalan.MapFromArray(arr) => {
        map.selfType match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val arr_ = symMirr(arr).asInstanceOf[lms.Exp[Array[(k, v)]]]
                val exp = lms.mapFromArray[k, v](arr_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case map@scalan.MapUsingFunc(count, lambdaSym@scalan.Def(lambda: scalan.Lambda[_, _])) => {
        map.selfType match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val n = symMirr(count).asInstanceOf[lms.Exp[Int]]
                val f = mirrorLambdaToLmsFunc[Int, (k, v)](m)(lambda.asInstanceOf[scalan.Lambda[Int, (k, v)]])
                val exp = lms.mapUsingFunc[k, v](n, f)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
              }
            }
          }
        }
      }
      case map@scalan.EmptyMap() => {
        map.selfType match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val exp = lms.emptyMap[k, v]()(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case map@scalan.MakeMap(ctx) => {
        map.selfType match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val exp = lms.emptyMap[k, v]()(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapUnion(left, right) => {
        left.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val left_ = symMirr(left).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val right_ = symMirr(right).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapUnion[k, v](left_, right_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapDifference(left, right) => {
        left.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val left_ = symMirr(left).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val right_ = symMirr(right).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapDifference[k, v](left_, right_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case tk@scalan.MapTransformValues(map, lamSym@scalan.Def(lambda: scalan.Lambda[_, _])) => {
        (map.elem, tk.selfType) match {
          case (in: scalan.MMapElem[k, v], out: scalan.MMapElem[_, t]) => {
            (scalan.createManifest(in.eKey), scalan.createManifest(in.eValue), scalan.createManifest(out.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v], mT: Manifest[t]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val f = mirrorLambdaToLmsFunc[v, t](m)(lambda.asInstanceOf[scalan.Lambda[v, t]])
                val exp = lms.mapTransformValues[k, v, t](map_, f)(mK, mV, mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, f)))
              }
            }
          }
        }
      }
      case scalan.MapReduce(left, right, reduceSym@scalan.Def(reduce: scalan.Lambda[_, _])) => {
        left.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val left_ = symMirr(left).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val right_ = symMirr(right).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[scalan.Lambda[(v, v), v]])
                val exp = lms.mapReduce[k, v](left_, right_, reduce_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case scalan.MapJoin(left, right) => {
        (left.elem, right.elem) match {
          case (e1: scalan.MMapElem[k1, v1], e2: scalan.MMapElem[k2, v2]) => {
            (scalan.createManifest(e1.eKey), scalan.createManifest(e1.eValue), scalan.createManifest(e2.eValue)) match {
              case (mK: Manifest[k1], mV1: Manifest[v1], mV2: Manifest[v2]) => {
                val left_ = symMirr(left).asInstanceOf[lms.Exp[HashMap[k1, v1]]]
                val right_ = symMirr(right).asInstanceOf[lms.Exp[HashMap[k1, v2]]]
                val exp = lms.mapJoin[k1, v1, v2](left_, right_)(mK, mV1, mV2)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapContains(map, key) => {
        map.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val key_ = symMirr(key).asInstanceOf[lms.Exp[k]]
                val exp = lms.mapContains[k, v](map_, key_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapApply(map, key) => {
        map.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val key_ = symMirr(key).asInstanceOf[lms.Exp[k]]
                val exp = lms.mapApply[k, v](map_, key_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case maf@scalan.MapApplyIf(map, key, s1@scalan.Def(exists: scalan.Lambda[_, _]), s2@scalan.Def(otherwise: scalan.Lambda[_, _])) => {
        (maf.selfType, map.elem) match {
          case (eT: scalan.Elem[t], eM: scalan.MMapElem[k, v]) => {
            (scalan.createManifest(eT), scalan.createManifest(eM.eKey), scalan.createManifest(eM.eValue)) match {
              case (mT: Manifest[t], mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val key_ = symMirr(key).asInstanceOf[lms.Exp[k]]
                val exists_ = mirrorLambdaToLmsFunc[v, t](m)(exists.asInstanceOf[scalan.Lambda[v, t]])
                val otherwise_ = mirrorLambdaToLmsFunc[Unit, t](m)(otherwise.asInstanceOf[scalan.Lambda[Unit, t]])
                val exp = lms.mapApplyIf[k, v, t](map_, key_, exists_, otherwise_)(mK, mV, mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((s1, exists_)) + ((s2, otherwise_)))
              }
            }
          }
        }
      }
      case scalan.MapUpdate(map, key, value) => {
        map.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val key_ = symMirr(key).asInstanceOf[lms.Exp[k]]
                val value_ = symMirr(value).asInstanceOf[lms.Exp[v]]
                val exp = lms.mapUpdate[k, v](map_, key_, value_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapSize(map) => {
        map.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapSize[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapToArray(map) => {
        map.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapToArray[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapKeys(map) => {
        map.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapKeys[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case scalan.MapValues(map) => {
        map.elem match {
          case elem: scalan.MMapElem[k, v] => {
            (scalan.createManifest(elem.eKey), scalan.createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapValues[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }


      case apply@scalan.ArrayApply(xs, ind) =>
        scalan.createManifest(apply.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val ind_ = symMirr(ind).asInstanceOf[lms.Exp[Int]]
            val exp = lms.arrayGet[a](xs_, ind_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ArrayApplyMany(xs, idxs) =>
        xs.elem match {
          case el: scalan.ArrayElem[_] =>
            scalan.createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
                val idxs_ = symMirr(idxs).asInstanceOf[lms.Exp[Array[Int]]]
                val exp = lms.arrayGather[a](xs_, idxs_)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case scalan.ArrayLength(xs) =>
        scalan.createManifest(xs.elem) match {
          case mA: Manifest[a] =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arrayLength[a](xs_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ArrayRangeFrom0(n) =>
        val n_ = symMirr(n).asInstanceOf[lms.Exp[Int]]
        val exp = lms.indexRangeD(n_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)

      case scalan.ArraySort(arg, o) => {
        arg.elem match {
          case (el: scalan.ArrayElem[_]) =>
            scalan.createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val arg_ = symMirr(arg).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arraySort[a](arg_)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      }
      case sort@scalan.ArraySortBy(arg, lambdaSym@scalan.Def(by: scalan.Lambda[_, b]), o) => {
        sort.selfType match {
          case (el: scalan.ArrayElem[a]) => {
            (scalan.createManifest(el.eItem), scalan.createManifest(by.eB)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val by_ = mirrorLambdaToLmsFunc[a, b](m)(by.asInstanceOf[scalan.Lambda[a, b]])
                val arg_ = symMirr(arg).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arraySortBy[a, b](arg_, by_)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, by_)))
            }
          }
        }
      }
      case gby@scalan.ArrayGroupBy(arg, lambdaSym@scalan.Def(by: scalan.Lambda[_, _])) => {
        (arg.elem, gby.selfType) match {
          case (ae: scalan.ArrayElem[a], me: scalan.MMapElem[k, v]) => {
            (scalan.createManifest(ae.eItem), scalan.createManifest(me.eKey)) match {
              case (mA: Manifest[a], mK: Manifest[k]) => {
                val by_ = mirrorLambdaToLmsFunc[a, k](m)(by.asInstanceOf[scalan.Lambda[a, k]])
                val arg_ = symMirr(arg).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arrayGroupBy[a, k](arg_, by_)(mA, mK)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, by_)))
              }
            }
          }
        }
      }
      case sum@scalan.ArraySum(xs, n) => {
        scalan.createManifest(sum.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arraySum[a](xs_)(mA, n.asInstanceOf[Numeric[a]])
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      }
      case min@scalan.ArrayMin(xs, o) => {
        scalan.createManifest(min.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arrayMin[a](xs_)(mA, o.asInstanceOf[Ordering[a]])
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      }
      case max@scalan.ArrayMax(xs, o) => {
        scalan.createManifest(max.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arrayMax[a](xs_)(mA, o.asInstanceOf[Ordering[a]])
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      }
      case scalan.ArrayAvg(xs, n) => {
        xs.elem match {
          case (el: scalan.ArrayElem[a]) => {
            scalan.createManifest(el.eItem) match {
              case (mA: Manifest[a]) => {
                val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arrayAvg[a](xs_)(mA, n.asInstanceOf[Numeric[a]])
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }

      case scalan.ArrayZip(arg1, arg2) =>
        (arg1.elem, arg2.elem) match {
          case (el1: scalan.ArrayElem[_], el2: scalan.ArrayElem[_]) =>
            (scalan.createManifest(el1.eItem), scalan.createManifest(el2.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[Array[a]]]
                val arg2_ = symMirr(arg2).asInstanceOf[lms.Exp[Array[b]]]
                val exp = lms.opZip[a, b](arg1_, arg2_)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case map@scalan.ArrayMap(source, lambdaSym@scalan.Def(lam: scalan.Lambda[_, _])) =>
        (source.elem, map.selfType) match {
          case (el: scalan.ArrayElem[_], el1: scalan.ArrayElem[_]) =>
            (scalan.createManifest(el.eItem), scalan.createManifest(el1.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val f = mirrorLambdaToLmsFunc[a, b](m)(lam.asInstanceOf[scalan.Lambda[a, b]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.mapArray[a, b](lmsSource, f)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }

      case map@scalan.ArrayFlatMap(source, lambdaSym@scalan.Def(lam: scalan.Lambda[_, _])) => {
        (source.elem, map.selfType) match {
          case (el: scalan.ArrayElem[_], el1: scalan.ArrayElem[_]) =>
            (scalan.createManifest(el.eItem), scalan.createManifest(el1.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val f = mirrorLambdaToLmsFunc[a, Array[b]](m)(lam.asInstanceOf[scalan.Lambda[a, Array[b]]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.flatMapArray[a, b](lmsSource, f)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }
      }

      case filter@scalan.ArrayFilter(source, lambdaSym@scalan.Def(lam: scalan.Lambda[_, _])) =>
        filter.selfType match {
          case el: scalan.ArrayElem[_] =>
            scalan.createManifest(el.eItem) match {
              case mA: Manifest[a] =>
                val f = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[scalan.Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.filterArray[a](lmsSource, f)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }

      case find@scalan.ArrayFind(source, lambdaSym@scalan.Def(lam: scalan.Lambda[_, _])) => {
        (source.elem) match {
          case (el: scalan.ArrayElem[_]) => {
            (scalan.createManifest(el.eItem)) match {
              case (mA: Manifest[a]) =>
                val f = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[scalan.Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.findArray[a](lmsSource, f)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
          }
        }
      }
      case scalan.ArrayCount(source, lambdaSym@scalan.Def(lam: scalan.Lambda[_, _])) => {
        (source.elem) match {
          case (el: scalan.ArrayElem[_]) =>
            (scalan.createManifest(el.eItem)) match {
              case (mA: Manifest[a]) =>
                val f = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[scalan.Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.countArray[a](lmsSource, f)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }
      }
      case mr@scalan.ArrayMapReduce(scalan.Def(range: scalan.ArrayRangeFrom0), mapSym@scalan.Def(map: scalan.Lambda[_, _]), reduceSym@scalan.Def(reduce: scalan.Lambda[_, _])) => {
        (mr.selfType) match {
          case (me: scalan.MMapElem[k, v]) => {
            (scalan.createManifest(me.eKey), scalan.createManifest(me.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val n_ = symMirr(range.n).asInstanceOf[lms.Exp[Int]]
                val map_ = mirrorLambdaToLmsFunc[Int, (k, v)](m)(map.asInstanceOf[scalan.Lambda[Int, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[scalan.Lambda[(v, v), v]])
                val exp = lms.rangeMapReduce(n_, map_, reduce_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((mapSym, map_)) + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case mr@scalan.ArrayMapReduce(scalan.Def(scalan.ArrayFilter(scalan.Def(scalan.ArrayMap(scalan.Def(range: scalan.ArrayRangeFrom0), map1Sym@scalan.Def(map1: scalan.Lambda[_, _]))), filterSym@scalan.Def(filter: scalan.Lambda[_, _]))),
      map2Sym@scalan.Def(map2: scalan.Lambda[_, _]), reduceSym@scalan.Def(reduce: scalan.Lambda[_, _])) => {
        (map1.eB, mr.selfType) match {
          case (ma: scalan.Elem[a], me: scalan.MMapElem[k, v]) => {
            (scalan.createManifest(ma), scalan.createManifest(me.eKey), scalan.createManifest(me.eValue)) match {
              case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) => {
                val n_ = symMirr(range.n).asInstanceOf[lms.Exp[Int]]
                val map1_ = mirrorLambdaToLmsFunc[Int, a](m)(map1.asInstanceOf[scalan.Lambda[Int, a]])
                val filter_ = mirrorLambdaToLmsFunc[a, Boolean](m)(filter.asInstanceOf[scalan.Lambda[a, Boolean]])
                val map2_ = mirrorLambdaToLmsFunc[a, (k, v)](m)(map2.asInstanceOf[scalan.Lambda[a, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[scalan.Lambda[(v, v), v]])
                val exp = lms.rangeFilterMapReduce(n_, map1_, filter_, map2_, reduce_)(mA, mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((map1Sym, map1_)) + ((map2Sym, map2_)) + ((filterSym, filter_)) + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case mr@scalan.ArrayMapReduce(scalan.Def(scalan.ArrayFilter(source, filterSym@scalan.Def(filter: scalan.Lambda[_, _]))), mapSym@scalan.Def(map: scalan.Lambda[_, _]), reduceSym@scalan.Def(reduce: scalan.Lambda[_, _])) => {
        (source.elem, mr.selfType) match {
          case (ae: scalan.ArrayElem[a], me: scalan.MMapElem[k, v]) => {
            (scalan.createManifest(ae.eItem), scalan.createManifest(me.eKey), scalan.createManifest(me.eValue)) match {
              case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) => {
                val source_ = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val filter_ = mirrorLambdaToLmsFunc[a, Boolean](m)(filter.asInstanceOf[scalan.Lambda[a, Boolean]])
                val map_ = mirrorLambdaToLmsFunc[a, (k, v)](m)(map.asInstanceOf[scalan.Lambda[a, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[scalan.Lambda[(v, v), v]])
                val exp = lms.filterMapReduce(source_, filter_, map_, reduce_)(mA, mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((mapSym, map_)) + ((reduceSym, reduce_)) + ((filterSym, filter_)))
              }
            }
          }
        }
      }
      case mr@scalan.ArrayMapReduce(source, mapSym@scalan.Def(map: scalan.Lambda[_, _]), reduceSym@scalan.Def(reduce: scalan.Lambda[_, _])) => {
        (source.elem, mr.selfType) match {
          case (ae: scalan.ArrayElem[a], me: scalan.MMapElem[k, v]) => {
            (scalan.createManifest(ae.eItem), scalan.createManifest(me.eKey), scalan.createManifest(me.eValue)) match {
              case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) => {
                val source_ = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val map_ = mirrorLambdaToLmsFunc[a, (k, v)](m)(map.asInstanceOf[scalan.Lambda[a, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[scalan.Lambda[(v, v), v]])
                val exp = lms.arrayMapReduce(source_, map_, reduce_)(mA, mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((mapSym, map_)) + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case f@scalan.ArrayFold(source, init, stepSym@scalan.Def(step: scalan.Lambda[_, _])) => {
        (f.selfType, source.elem) match {
          case (e: scalan.Elem[s], ae: scalan.ArrayElem[a]) => {
            (scalan.createManifest(e), scalan.createManifest(ae.eItem)) match {
              case (mS: Manifest[s], mA: Manifest[a]) => {
                val src = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val state = symMirr(init).asInstanceOf[lms.Exp[s]]
                val func = mirrorLambdaToLmsFunc[(s, a), s](m)(step.asInstanceOf[scalan.Lambda[(s, a), s]])
                val exp = lms.fold[a, s](src, state, func)(mA, mS)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((stepSym, func)))
              }
            }
          }
        }
      }
      case sum@scalan.ArraySumBy(source, lamSym@scalan.Def(f: scalan.Lambda[_, _]), n) => {
        (sum.selfType, source.elem) match {
          case (e: scalan.Elem[s], ae: scalan.ArrayElem[a]) => {
            (scalan.createManifest(e), scalan.createManifest(ae.eItem)) match {
              case (mS: Manifest[s], mA: Manifest[a]) => {
                val src = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val func = mirrorLambdaToLmsFunc[a, s](m)(f.asInstanceOf[scalan.Lambda[a, s]])
                val exp = lms.sumBy[a, s](src, func)(mA, mS, n.asInstanceOf[Numeric[s]])
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, func)))
              }
            }
          }
        }
      }
      case scalan.ParallelExecute(nJobs, lamSym@scalan.Def(f: scalan.Lambda[_, _])) => {
        f.eB match {
          case el: scalan.Elem[b] => {
            scalan.createManifest(el) match {
              case (mB: Manifest[b]) => {
                val n = symMirr(nJobs).asInstanceOf[lms.Exp[Int]]
                val func = mirrorLambdaToLmsFunc[Int, b](m)(f.asInstanceOf[scalan.Lambda[Int, b]])
                val exp = lms.parallelExecute[b](n, func)(mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, func)))
              }
            }
          }
        }
      }

      case scalan.ArrayUpdate(xs, index, value) =>
        xs.elem match {
          case el: scalan.ArrayElem[a] =>
            val mA = scalan.createManifest(el.eItem).asInstanceOf[Manifest[a]]
            val lmsXs = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val lmsIndex = symMirr(index).asInstanceOf[lms.Exp[Int]]
            val lmsValue = symMirr(value).asInstanceOf[lms.Exp[a]]
            val exp = lms.updateArray(lmsXs, lmsIndex, lmsValue)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      /* This is reduce */
      case scalan.ArrayReduce(source, monoid) => {
        source.elem match {
          case el: scalan.ArrayElem[_] => {
            scalan.createManifest(el.eItem) match {
              case (mA: Manifest[a]) => {
                val src = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                if (monoid.opName == "+") {
                  val exp = lms.sum[a](src)(mA)
                  (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
                } else {
                  monoid.append match {
                    case opSym@scalan.Def(lambda: scalan.Lambda[_, _]) => {
                      val zero = symMirr(monoid.zero).asInstanceOf[lms.Exp[a]]
                      val op = mirrorLambdaToLmsFunc[(a, a), a](m)(lambda.asInstanceOf[scalan.Lambda[(a, a), a]])
                      val exp = lms.reduce[a](src, zero, op)(mA)
                      (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((opSym, op)))
                    }
                  }
                }
              }
            }
          }
        }
      }

      case fun@scalan.ArrayFold(source, init, lambdaSym@scalan.Def(lam: scalan.Lambda[_, _])) =>
        source.elem match {
          case el: scalan.ArrayElem[_] =>
            (scalan.createManifest(el.eItem), scalan.createManifest(fun.selfType)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val lambdaF = mirrorLambdaToLmsFunc[(b, a), b](m)(lam.asInstanceOf[scalan.Lambda[(b, a), b]])
                val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val lmsInit = symMirr(init).asInstanceOf[lms.Exp[b]]
                val exp = lms.foldArray[a, b](lmsSource, lmsInit, lambdaF)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, lambdaF)))
            }
        }

      case scalan.ArrayStride(xs, start, length, stride) =>
        xs.elem match {
          case el: scalan.ArrayElem[a] =>
            val mA = scalan.createManifest(el.eItem).asInstanceOf[Manifest[a]]
            val lmsXs = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val lmsStart = symMirr(start).asInstanceOf[lms.Exp[Int]]
            val lmsLength = symMirr(length).asInstanceOf[lms.Exp[Int]]
            val lmsStride = symMirr(stride).asInstanceOf[lms.Exp[Int]]
            val exp = lms.strideArray(lmsXs, lmsStart, lmsLength, lmsStride)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case lr@scalan.ListMap(list, lamSym@scalan.Def(lam: scalan.Lambda[_, _])) =>
        (scalan.createManifest(list.elem), scalan.createManifest(lam.eB)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          val lambdaF = mirrorLambdaToLmsFunc[a, b](m)(lam.asInstanceOf[scalan.Lambda[a, b]])
          val exp = lms.listMap[a, b](symMirr(list).asInstanceOf[lms.Exp[List[a]]], lambdaF)(mA, mB)
          (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, lambdaF)))
      }

      case lr@scalan.ListRangeFrom0(len) =>
        scalan.createManifest(lr.eT) match {
          case mA: Manifest[a] =>
            val exp = lms.listRangeFrom0[a](symMirr(len).asInstanceOf[lms.Exp[Int]])(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case loop@scalan.LoopUntil(state, stepSym@scalan.Def(step: scalan.Lambda[_, _]), condSym@scalan.Def(cond: scalan.Lambda[_, _])) => {
        scalan.createManifest(loop.selfType) match {
          case (mA: Manifest[a]) => {
            val cond_ = mirrorLambdaToLmsFunc[a, Boolean](m)(cond.asInstanceOf[scalan.Lambda[a, Boolean]])
            val step_ = mirrorLambdaToLmsFunc[a, a](m)(step.asInstanceOf[scalan.Lambda[a, a]])
            val state_ = symMirr(state).asInstanceOf[lms.Exp[a]]
            val exp = lms.loopUntil[a](state_, cond_, step_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((condSym, cond_)) + ((stepSym, step_)))
          }
        }
      }

      case scalan.ListHead(xs) =>
        import scalan.extendListElement
        scalan.createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] =>
            implicit val imA = mA
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_head[a](ls)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ListTail(xs) =>
        import scalan.extendListElement
        scalan.createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] =>
            implicit val imA = mA
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_tail[a](ls)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ListCons(x, xs) ⇒
        import scalan.extendListElement
        scalan.createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] ⇒
            implicit val imA = mA
            val l = symMirr(x).asInstanceOf[lms.Exp[a]]
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_prepend[a](ls, l)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ListConcat(xs, ys) =>
        import scalan.extendListElement
        scalan.createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] =>
            implicit val imA = mA
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val ks = symMirr(ys).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_concat[a](ls, ks)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }
    }

    tt
  }
}
