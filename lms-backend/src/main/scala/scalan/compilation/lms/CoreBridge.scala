package scalan
package compilation.lms

import java.lang.reflect.Method
import java.util.HashMap

import scalan.compilation.language.{CoreMethodMapping, MethodMapping, Interpreter}

trait CoreBridge extends LmsBridge with Interpreter with CoreMethodMapping { self: ScalanCtxExp =>

  val lms: CoreLmsBackendBase

  override def defTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]) =
    coreDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)

  def coreDefTransformer[T](m: LmsMirror, fromGraph: AstGraph, tp: TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = tp.sym

    val tt: DefTransformer = {
      case _: CompanionBase[_] =>
        // ignore companion objects
        m

      case lam: Lambda[a, b] =>
        val mA = createManifest(lam.x.elem).asInstanceOf[Manifest[a]]
        val mB = createManifest(lam.y.elem).asInstanceOf[Manifest[b]]
        val f = mirrorLambdaToLmsFunc[a, b](m)(lam)
        val fun = lms.fun(f)(mA, mB)
        (exps :+ fun, symMirr + ((sym, fun)), funcMirr + ((sym, f)))

      case MethodCall(receiver, method, args, _) =>
        val exp = transformMethodCall(symMirr, receiver, method, args)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)

      case lr@NewObject(aClass, args, _) =>
        Manifest.classType(aClass) match {
          case (mA: Manifest[a]) =>
            val exp = lms.newObj[a](aClass.getCanonicalName, args.map(v => symMirr(v.asInstanceOf[Exp[_]])))(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case Apply(f, x) =>
        (createManifest(f.elem.eDom), createManifest(f.elem.eRange)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val fun = symMirr(f).asInstanceOf[lms.Exp[a => b]]
            val arg = symMirr(x).asInstanceOf[lms.Exp[a]]
            val exp = lms.doApply[a, b](fun, arg)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case c@Const(_) =>
        createManifest(c.selfType) match {
          case mA: Manifest[a] =>
            val x = c.x.asInstanceOf[a]
            val exp = lms.unitD(x)(mA)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case d@Left(l) =>
        (createManifest(d.selfType.eLeft), createManifest(d.selfType.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val left = symMirr(l).asInstanceOf[lms.Exp[a]]
            val exp = lms.make_left[a, b](left)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case d@Right(r) =>
        (createManifest(d.selfType.eLeft), createManifest(d.selfType.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val right = symMirr(r).asInstanceOf[lms.Exp[b]]
            val exp = lms.make_right[a, b](right)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case IsLeft(s) =>
        (createManifest(s.elem.eLeft), createManifest(s.elem.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val sum = symMirr(s).asInstanceOf[lms.Exp[Either[a, b]]]
            val exp = lms.make_isLeft(sum)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case IsRight(s) =>
        (createManifest(s.elem.eLeft), createManifest(s.elem.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            implicit val (imA, imB) = (mA, mB)
            val sum = symMirr(s).asInstanceOf[lms.Exp[Either[a, b]]]
            val exp = lms.make_isRight(sum)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case SumFold(s, l, r) =>
        (createManifest(s.elem.eLeft), createManifest(s.elem.eRight), createManifest(l.elem.eRange)) match {
          case (mA: Manifest[a], mB: Manifest[b], mR: Manifest[r]) =>
            implicit val (imA, imB, imR) = (mA, mB, mR)
            val sum = symMirr(s).asInstanceOf[lms.Exp[Either[a, b]]]
            val left = symMirr(l).asInstanceOf[lms.Exp[a => r]]
            val right = symMirr(r).asInstanceOf[lms.Exp[b => r]]
            val exp = lms.make_fold(sum, left, right)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case Tup(fst, snd) =>
        (createManifest(fst.elem), createManifest(snd.elem)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            val first = symMirr(fst).asInstanceOf[lms.Exp[a]]
            val second = symMirr(snd).asInstanceOf[lms.Exp[b]]
            val exp = lms.tuple[a, b](first, second)(mA, mB)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case First(tuple) =>
        tuple.elem match {
          case pe: PairElem[_, _] =>
            (createManifest(pe.eFst), createManifest(pe.eSnd)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val tup = symMirr(tuple).asInstanceOf[lms.Exp[(a, b)]]
                val exp = lms.first[a, b](tup)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case Second(tuple) =>
        tuple.elem match {
          case pe: PairElem[_, _] =>
            (createManifest(pe.eFst), createManifest(pe.eSnd)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val tup = symMirr(tuple).asInstanceOf[lms.Exp[(a, b)]]
                val exp = lms.second[a, b](tup)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case ApplyUnOp(op, arg1) => {
        createManifest(arg1.elem) match {
          case (mA: Manifest[a]) => {
            val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[a]]
            val exp: lms.Exp[_] = op.asInstanceOf[UnOp[a, _]] match {
              case Not => lms.Not(arg1_.asInstanceOf[lms.Exp[Boolean]])
              case NumericNegate(n) => lms.Neg(arg1_)(mA, n.asInstanceOf[Numeric[a]])
              case NumericToDouble(n) =>
                mA match {
                  case Manifest.Int => lms.IntToDouble(arg1_.asInstanceOf[lms.Exp[Int]])
                  case Manifest.Float => lms.FloatToDouble(arg1_.asInstanceOf[lms.Exp[Float]])
                  case Manifest.Double => arg1_
                }
              case NumericToFloat(n) =>
                mA match {
                  case Manifest.Int => lms.IntToFloat(arg1_.asInstanceOf[lms.Exp[Int]])
                  case Manifest.Double => lms.DoubleToFloat(arg1_.asInstanceOf[lms.Exp[Double]])
                  case Manifest.Float => arg1_
                }
              case NumericToInt(n) =>
                mA match {
                  case Manifest.Float => lms.FloatToInt(arg1_.asInstanceOf[lms.Exp[Float]])
                  case Manifest.Double => lms.DoubleToInt(arg1_.asInstanceOf[lms.Exp[Double]])
                  case Manifest.Int => arg1_
                }
              case NumericToString() => lms.ToString(arg1_)
              case HashCode() => lms.hashCode(arg1_)
              case StringToInt() => lms.stringToInt(arg1_.asInstanceOf[lms.Exp[String]])
              case StringToDouble() => lms.stringToDouble(arg1_.asInstanceOf[lms.Exp[String]])
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
      case StringSubstr(str, start, end) => {
        val str_ = symMirr(str).asInstanceOf[lms.Exp[String]]
        val start_ = symMirr(start).asInstanceOf[lms.Exp[Int]]
        val end_ = symMirr(end).asInstanceOf[lms.Exp[Int]]
        val exp = lms.substring(str_, start_, end_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      }
      case StringApply(str, index) => {
        val str_ = symMirr(str).asInstanceOf[lms.Exp[String]]
        val index_ = symMirr(index).asInstanceOf[lms.Exp[Int]]
        val exp = lms.charAt(str_, index_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      }
      case ApplyBinOp(op, arg1, arg2) =>
        createManifest(arg1.elem) match {
          case (mA: Manifest[a]) =>
            val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[a]]
            val arg2_ = symMirr(arg2).asInstanceOf[lms.Exp[a]]
            val exp = op.asInstanceOf[BinOp[a, _]] match {
              case NumericTimes(n) =>
                lms.opMult(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case NumericPlus(n) =>
                lms.opPlus(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case NumericMinus(n) =>
                lms.opMinus(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case IntegralDivide(n) =>
                lms.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case IntegralMod(n) =>
                mA match {
                  case Manifest.Int =>
                    lms.opMod(arg1_.asInstanceOf[lms.Exp[Int]], arg2_.asInstanceOf[lms.Exp[Int]])
                  case _ =>
                    throw new IllegalStateException(s"LMS only supports mod operation for Int, got $mA instead")
                }
              case FractionalDivide(n) =>
                lms.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case Equals() =>
                lms.opEq[a](arg1_, arg2_)(mA)
              case OrderingLT(ord) =>
                lms.LT[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case OrderingLTEQ(ord) =>
                lms.LTEQ[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case OrderingGT(ord) =>
                lms.GT[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case OrderingGTEQ(ord) =>
                lms.GTEQ[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case OrderingMax(ord) =>
                lms.Max[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case OrderingMin(ord) =>
                lms.Min[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case And =>
                lms.And(arg1_.asInstanceOf[lms.Exp[Boolean]], arg2_.asInstanceOf[lms.Exp[Boolean]])
              case Or =>
                lms.Or(arg1_.asInstanceOf[lms.Exp[Boolean]], arg2_.asInstanceOf[lms.Exp[Boolean]])
              case StringConcat() =>
                lms.stringConcat(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case StringContains() =>
                lms.stringContains(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case StringStartsWith() =>
                lms.stringStartsWith(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case StringEndsWith() =>
                lms.stringEndsWith(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case StringMatches() =>
                lms.stringMatches(arg1_.asInstanceOf[lms.Exp[String]], arg2_.asInstanceOf[lms.Exp[String]])
              case MathPow =>
                lms.Pow(arg1_.asInstanceOf[lms.Exp[Double]], arg2_.asInstanceOf[lms.Exp[Double]])
            }
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case ThrowException(msg) => {
        val msg_ = symMirr(msg).asInstanceOf[lms.Exp[String]]
        val exp = lms.throwException(msg_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      }
      case Semicolon(left, right) => {
        (createManifest(left.elem), createManifest(right.elem)) match {
          case (mA: Manifest[a], mB: Manifest[b]) => {
            val left_ = symMirr(left).asInstanceOf[lms.Exp[a]]
            val right_ = symMirr(right).asInstanceOf[lms.Exp[b]]
            val exp = lms.block(left_, right_)(mA, mB)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }
      case i@IfThenElse(cond, iftrue, iffalse) =>
        createManifest(i.selfType) match {
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

      case loop@LoopUntil(state, stepSym@Def(step: Lambda[_, _]), condSym@Def(cond: Lambda[_, _])) => {
        createManifest(loop.selfType) match {
          case (mA: Manifest[a]) => {
            val cond_ = mirrorLambdaToLmsFunc[a, Boolean](m)(cond.asInstanceOf[Lambda[a, Boolean]])
            val step_ = mirrorLambdaToLmsFunc[a, a](m)(step.asInstanceOf[Lambda[a, a]])
            val state_ = symMirr(state).asInstanceOf[lms.Exp[a]]
            val exp = lms.loopUntil[a](state_, cond_, step_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((condSym, cond_)) + ((stepSym, step_)))
          }
        }
      }
      //
      // Array Buffer
      //
      case buf@ArrayBufferEmpty() => {
        buf.selfType match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val exp = lms.emptyArrayBuffer[t]()(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case buf@MakeArrayBuffer(ctx) => {
        buf.selfType match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val exp = lms.emptyArrayBuffer[t]()(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case buf@ArrayBufferFromElem(e) => {
        buf.selfType match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val v = symMirr(e).asInstanceOf[lms.Exp[t]]
                val exp = lms.arrayBufferFromElem(v)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case ArrayBufferApply(buf, i) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
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
      case ArrayBufferLength(buf) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val exp = lms.arrayBufferLength(b_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case res@ArrayBufferMap(buf, lambdaSym@Def(lambda: Lambda[_, _])) => {
        (res.selfType, buf.elem) match {
          case (from: ArrayBufferElem[a], to: ArrayBufferElem[b]) => {
            (createManifest(from.eItem), createManifest(to.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[a]]]
                val f_ = mirrorLambdaToLmsFunc[a, b](m)(lambda.asInstanceOf[Lambda[a, b]])
                val exp = lms.arrayBufferMap(b_, f_)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f_)))
              }
            }
          }
        }
      }
      case ArrayBufferUpdate(buf, i, v) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
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
      case ArrayBufferInsert(buf, i, v) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
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
      case ArrayBufferAppend(buf, v) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
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
      case ArrayBufferAppendArray(buf, a) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
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
      case ArrayBufferRemove(buf, i, n) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
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
      case ArrayBufferReset(buf) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val exp = lms.arrayBufferReset(b_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case ArrayBufferToArray(buf) => {
        buf.elem match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val b_ = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
                val exp = lms.arrayBufferToArray(b_)(mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case ab@ArrayBufferRep(buf) => {
        ab.selfType match {
          case elem: ArrayBufferElem[t] => {
            val exp = symMirr(buf).asInstanceOf[lms.Exp[scala.collection.mutable.ArrayBuilder[t]]]
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }
      case buf@ArrayBufferUsingFunc(count, lambdaSym@Def(lambda: Lambda[_, _])) => {
        buf.selfType match {
          case elem: ArrayBufferElem[t] => {
            createManifest(elem.eItem) match {
              case mT: Manifest[t] => {
                val n = symMirr(count).asInstanceOf[lms.Exp[Int]]
                val f = mirrorLambdaToLmsFunc[Int, t](m)(lambda.asInstanceOf[Lambda[Int, t]])
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
      case AppendMultiMap(map, key, value) => {
        (key.elem, value.elem) match {
          case (eK: Elem[k], eV: Elem[v]) => {
            (createManifest(eK), createManifest(eV)) match {
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
      case pm@VarMM(map) => {
        pm.selfType match {
          case elem: MMapElem[k, v] => {
            val exp = symMirr(map).asInstanceOf[lms.Exp[MMap[k, v]]]
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }

      case map@MapFromArray(arr) => {
        map.selfType match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val arr_ = symMirr(arr).asInstanceOf[lms.Exp[Array[(k, v)]]]
                val exp = lms.mapFromArray[k, v](arr_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case map@MapUsingFunc(count, lambdaSym@Def(lambda: Lambda[_, _])) => {
        map.selfType match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val n = symMirr(count).asInstanceOf[lms.Exp[Int]]
                val f = mirrorLambdaToLmsFunc[Int, (k, v)](m)(lambda.asInstanceOf[Lambda[Int, (k, v)]])
                val exp = lms.mapUsingFunc[k, v](n, f)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
              }
            }
          }
        }
      }
      case map@EmptyMap() => {
        map.selfType match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val exp = lms.emptyMap[k, v]()(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case map@MakeMap(ctx) => {
        map.selfType match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val exp = lms.emptyMap[k, v]()(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case MapUnion(left, right) => {
        left.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
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
      case MapDifference(left, right) => {
        left.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
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
      case tk@MapTransformValues(map, lamSym@Def(lambda: Lambda[_, _])) => {
        (map.elem, tk.selfType) match {
          case (in: MMapElem[k, v], out: MMapElem[_, t]) => {
            (createManifest(in.eKey), createManifest(in.eValue), createManifest(out.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v], mT: Manifest[t]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val f = mirrorLambdaToLmsFunc[v, t](m)(lambda.asInstanceOf[Lambda[v, t]])
                val exp = lms.mapTransformValues[k, v, t](map_, f)(mK, mV, mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, f)))
              }
            }
          }
        }
      }
      case MapReduce(left, right, reduceSym@Def(reduce: Lambda[_, _])) => {
        left.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val left_ = symMirr(left).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val right_ = symMirr(right).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[Lambda[(v, v), v]])
                val exp = lms.mapReduce[k, v](left_, right_, reduce_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case MapJoin(left, right) => {
        (left.elem, right.elem) match {
          case (e1: MMapElem[k1, v1], e2: MMapElem[k2, v2]) => {
            (createManifest(e1.eKey), createManifest(e1.eValue), createManifest(e2.eValue)) match {
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
      case MapContains(map, key) => {
        map.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
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
      case MapApply(map, key) => {
        map.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
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
      case maf@MapApplyIf(map, key, s1@Def(exists: Lambda[_, _]), s2@Def(otherwise: Lambda[_, _])) => {
        (maf.selfType, map.elem) match {
          case (eT: Elem[t], eM: MMapElem[k, v]) => {
            (createManifest(eT), createManifest(eM.eKey), createManifest(eM.eValue)) match {
              case (mT: Manifest[t], mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val key_ = symMirr(key).asInstanceOf[lms.Exp[k]]
                val exists_ = mirrorLambdaToLmsFunc[v, t](m)(exists.asInstanceOf[Lambda[v, t]])
                val otherwise_ = mirrorLambdaToLmsFunc[Unit, t](m)(otherwise.asInstanceOf[Lambda[Unit, t]])
                val exp = lms.mapApplyIf[k, v, t](map_, key_, exists_, otherwise_)(mK, mV, mT)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((s1, exists_)) + ((s2, otherwise_)))
              }
            }
          }
        }
      }
      case MapUpdate(map, key, value) => {
        map.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
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
      case MapSize(map) => {
        map.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapSize[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case MapToArray(map) => {
        map.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapToArray[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case MapKeys(map) => {
        map.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapKeys[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }
      case MapValues(map) => {
        map.elem match {
          case elem: MMapElem[k, v] => {
            (createManifest(elem.eKey), createManifest(elem.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val map_ = symMirr(map).asInstanceOf[lms.Exp[HashMap[k, v]]]
                val exp = lms.mapValues[k, v](map_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }


      case apply@ArrayApply(xs, ind) =>
        createManifest(apply.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val ind_ = symMirr(ind).asInstanceOf[lms.Exp[Int]]
            val exp = lms.arrayGet[a](xs_, ind_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case ArrayApplyMany(xs, idxs) =>
        xs.elem match {
          case el: ArrayElem[_] =>
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
                val idxs_ = symMirr(idxs).asInstanceOf[lms.Exp[Array[Int]]]
                val exp = lms.arrayGather[a](xs_, idxs_)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case ArrayLength(xs) =>
        createManifest(xs.elem) match {
          case mA: Manifest[a] =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arrayLength[a](xs_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case ArrayRangeFrom0(n) =>
        val n_ = symMirr(n).asInstanceOf[lms.Exp[Int]]
        val exp = lms.indexRangeD(n_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)

      case ArraySort(arg, o) => {
        arg.elem match {
          case (el: ArrayElem[_]) =>
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val arg_ = symMirr(arg).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arraySort[a](arg_)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      }
      case sort@ArraySortBy(arg, lambdaSym@Def(by: Lambda[_, b]), o) => {
        sort.selfType match {
          case (el: ArrayElem[a]) => {
            (createManifest(el.eItem), createManifest(by.eB)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val by_ = mirrorLambdaToLmsFunc[a, b](m)(by.asInstanceOf[Lambda[a, b]])
                val arg_ = symMirr(arg).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arraySortBy[a, b](arg_, by_)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, by_)))
            }
          }
        }
      }
      case gby@ArrayGroupBy(arg, lambdaSym@Def(by: Lambda[_, _])) => {
        (arg.elem, gby.selfType) match {
          case (ae: ArrayElem[a], me: MMapElem[k, v]) => {
            (createManifest(ae.eItem), createManifest(me.eKey)) match {
              case (mA: Manifest[a], mK: Manifest[k]) => {
                val by_ = mirrorLambdaToLmsFunc[a, k](m)(by.asInstanceOf[Lambda[a, k]])
                val arg_ = symMirr(arg).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arrayGroupBy[a, k](arg_, by_)(mA, mK)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, by_)))
              }
            }
          }
        }
      }
      case sum@ArraySum(xs, n) => {
        createManifest(sum.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arraySum[a](xs_)(mA, n.asInstanceOf[Numeric[a]])
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      }
      case min@ArrayMin(xs, o) => {
        createManifest(min.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arrayMin[a](xs_)(mA, o.asInstanceOf[Ordering[a]])
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      }
      case max@ArrayMax(xs, o) => {
        createManifest(max.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arrayMax[a](xs_)(mA, o.asInstanceOf[Ordering[a]])
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      }
      case ArrayAvg(xs, n) => {
        xs.elem match {
          case (el: ArrayElem[a]) => {
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) => {
                val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.arrayAvg[a](xs_)(mA, n.asInstanceOf[Numeric[a]])
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
          }
        }
      }

      case ArrayZip(arg1, arg2) =>
        (arg1.elem, arg2.elem) match {
          case (el1: ArrayElem[_], el2: ArrayElem[_]) =>
            (createManifest(el1.eItem), createManifest(el2.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[Array[a]]]
                val arg2_ = symMirr(arg2).asInstanceOf[lms.Exp[Array[b]]]
                val exp = lms.opZip[a, b](arg1_, arg2_)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case map@ArrayMap(source, lambdaSym@Def(lam: Lambda[_, _])) =>
        (source.elem, map.selfType) match {
          case (el: ArrayElem[_], el1: ArrayElem[_]) =>
            (createManifest(el.eItem), createManifest(el1.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val f = mirrorLambdaToLmsFunc[a, b](m)(lam.asInstanceOf[Lambda[a, b]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.mapArray[a, b](lmsSource, f)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }

      case map@ArrayFlatMap(source, lambdaSym@Def(lam: Lambda[_, _])) => {
        (source.elem, map.selfType) match {
          case (el: ArrayElem[_], el1: ArrayElem[_]) =>
            (createManifest(el.eItem), createManifest(el1.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val f = mirrorLambdaToLmsFunc[a, Array[b]](m)(lam.asInstanceOf[Lambda[a, Array[b]]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.flatMapArray[a, b](lmsSource, f)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }
      }

      case filter@ArrayFilter(source, lambdaSym@Def(lam: Lambda[_, _])) =>
        filter.selfType match {
          case el: ArrayElem[_] =>
            createManifest(el.eItem) match {
              case mA: Manifest[a] =>
                val f = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.filterArray[a](lmsSource, f)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }

      case find@ArrayFind(source, lambdaSym@Def(lam: Lambda[_, _])) => {
        (source.elem) match {
          case (el: ArrayElem[_]) => {
            (createManifest(el.eItem)) match {
              case (mA: Manifest[a]) =>
                val f = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.findArray[a](lmsSource, f)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
          }
        }
      }
      case ArrayCount(source, lambdaSym@Def(lam: Lambda[_, _])) => {
        (source.elem) match {
          case (el: ArrayElem[_]) =>
            (createManifest(el.eItem)) match {
              case (mA: Manifest[a]) =>
                val f = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.countArray[a](lmsSource, f)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }
      }
      case mr@ArrayMapReduce(Def(range: ArrayRangeFrom0), mapSym@Def(map: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) => {
        (mr.selfType) match {
          case (me: MMapElem[k, v]) => {
            (createManifest(me.eKey), createManifest(me.eValue)) match {
              case (mK: Manifest[k], mV: Manifest[v]) => {
                val n_ = symMirr(range.n).asInstanceOf[lms.Exp[Int]]
                val map_ = mirrorLambdaToLmsFunc[Int, (k, v)](m)(map.asInstanceOf[Lambda[Int, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[Lambda[(v, v), v]])
                val exp = lms.rangeMapReduce(n_, map_, reduce_)(mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((mapSym, map_)) + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case mr@ArrayMapReduce(Def(ArrayFilter(Def(ArrayMap(Def(range: ArrayRangeFrom0), map1Sym@Def(map1: Lambda[_, _]))), filterSym@Def(filter: Lambda[_, _]))),
      map2Sym@Def(map2: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) => {
        (map1.eB, mr.selfType) match {
          case (ma: Elem[a], me: MMapElem[k, v]) => {
            (createManifest(ma), createManifest(me.eKey), createManifest(me.eValue)) match {
              case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) => {
                val n_ = symMirr(range.n).asInstanceOf[lms.Exp[Int]]
                val map1_ = mirrorLambdaToLmsFunc[Int, a](m)(map1.asInstanceOf[Lambda[Int, a]])
                val filter_ = mirrorLambdaToLmsFunc[a, Boolean](m)(filter.asInstanceOf[Lambda[a, Boolean]])
                val map2_ = mirrorLambdaToLmsFunc[a, (k, v)](m)(map2.asInstanceOf[Lambda[a, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[Lambda[(v, v), v]])
                val exp = lms.rangeFilterMapReduce(n_, map1_, filter_, map2_, reduce_)(mA, mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((map1Sym, map1_)) + ((map2Sym, map2_)) + ((filterSym, filter_)) + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case mr@ArrayMapReduce(Def(ArrayFilter(source, filterSym@Def(filter: Lambda[_, _]))), mapSym@Def(map: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) => {
        (source.elem, mr.selfType) match {
          case (ae: ArrayElem[a], me: MMapElem[k, v]) => {
            (createManifest(ae.eItem), createManifest(me.eKey), createManifest(me.eValue)) match {
              case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) => {
                val source_ = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val filter_ = mirrorLambdaToLmsFunc[a, Boolean](m)(filter.asInstanceOf[Lambda[a, Boolean]])
                val map_ = mirrorLambdaToLmsFunc[a, (k, v)](m)(map.asInstanceOf[Lambda[a, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[Lambda[(v, v), v]])
                val exp = lms.filterMapReduce(source_, filter_, map_, reduce_)(mA, mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((mapSym, map_)) + ((reduceSym, reduce_)) + ((filterSym, filter_)))
              }
            }
          }
        }
      }
      case mr@ArrayMapReduce(source, mapSym@Def(map: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) => {
        (source.elem, mr.selfType) match {
          case (ae: ArrayElem[a], me: MMapElem[k, v]) => {
            (createManifest(ae.eItem), createManifest(me.eKey), createManifest(me.eValue)) match {
              case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) => {
                val source_ = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val map_ = mirrorLambdaToLmsFunc[a, (k, v)](m)(map.asInstanceOf[Lambda[a, (k, v)]])
                val reduce_ = mirrorLambdaToLmsFunc[(v, v), v](m)(reduce.asInstanceOf[Lambda[(v, v), v]])
                val exp = lms.arrayMapReduce(source_, map_, reduce_)(mA, mK, mV)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((mapSym, map_)) + ((reduceSym, reduce_)))
              }
            }
          }
        }
      }
      case f@ArrayFold(source, init, stepSym@Def(step: Lambda[_, _])) => {
        (f.selfType, source.elem) match {
          case (e: Elem[s], ae: ArrayElem[a]) => {
            (createManifest(e), createManifest(ae.eItem)) match {
              case (mS: Manifest[s], mA: Manifest[a]) => {
                val src = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val state = symMirr(init).asInstanceOf[lms.Exp[s]]
                val func = mirrorLambdaToLmsFunc[(s, a), s](m)(step.asInstanceOf[Lambda[(s, a), s]])
                val exp = lms.fold[a, s](src, state, func)(mA, mS)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((stepSym, func)))
              }
            }
          }
        }
      }
      case sum@ArraySumBy(source, lamSym@Def(f: Lambda[_, _]), n) => {
        (sum.selfType, source.elem) match {
          case (e: Elem[s], ae: ArrayElem[a]) => {
            (createManifest(e), createManifest(ae.eItem)) match {
              case (mS: Manifest[s], mA: Manifest[a]) => {
                val src = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val func = mirrorLambdaToLmsFunc[a, s](m)(f.asInstanceOf[Lambda[a, s]])
                val exp = lms.sumBy[a, s](src, func)(mA, mS, n.asInstanceOf[Numeric[s]])
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, func)))
              }
            }
          }
        }
      }
      case ParallelExecute(nJobs, lamSym@Def(f: Lambda[_, _])) => {
        f.eB match {
          case el: Elem[b] => {
            createManifest(el) match {
              case (mB: Manifest[b]) => {
                val n = symMirr(nJobs).asInstanceOf[lms.Exp[Int]]
                val func = mirrorLambdaToLmsFunc[Int, b](m)(f.asInstanceOf[Lambda[Int, b]])
                val exp = lms.parallelExecute[b](n, func)(mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, func)))
              }
            }
          }
        }
      }

      case ArrayUpdate(xs, index, value) =>
        xs.elem match {
          case el: ArrayElem[a] =>
            val mA = createManifest(el.eItem).asInstanceOf[Manifest[a]]
            val lmsXs = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val lmsIndex = symMirr(index).asInstanceOf[lms.Exp[Int]]
            val lmsValue = symMirr(value).asInstanceOf[lms.Exp[a]]
            val exp = lms.updateArray(lmsXs, lmsIndex, lmsValue)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      /* This is reduce */
      case ArrayReduce(source, monoid) => {
        source.elem match {
          case el: ArrayElem[_] => {
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) => {
                val src = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                monoid.opName match {
                  case "+" =>
                    val exp = lms.sum[a](src)(mA)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
                  case _ =>
                    monoid.append match {
                      case opSym@Def(lambda: Lambda[_, _]) => {
                        val zero = symMirr(monoid.zero).asInstanceOf[lms.Exp[a]]
                        val op = mirrorLambdaToLmsFunc[(a, a), a](m)(lambda.asInstanceOf[Lambda[(a, a), a]])
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

      case ArrayScan(source, monoid) => {
        source.elem match {
          case el: ArrayElem[_] => {
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) => {
                val src = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                monoid.opName match {
                  case "+" =>
                    val exp = lms.sum[a](src)(mA)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
                  case _ =>
                    monoid.append match {
                      case opSym@Def(lambda: Lambda[_, _]) => {
                        val zero = symMirr(monoid.zero).asInstanceOf[lms.Exp[a]]
                        val op = mirrorLambdaToLmsFunc[(a, a), a](m)(lambda.asInstanceOf[Lambda[(a, a), a]])
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

      case ArrayStride(xs, start, length, stride) =>
        xs.elem match {
          case el: ArrayElem[a] =>
            val mA = createManifest(el.eItem).asInstanceOf[Manifest[a]]
            val lmsXs = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val lmsStart = symMirr(start).asInstanceOf[lms.Exp[Int]]
            val lmsLength = symMirr(length).asInstanceOf[lms.Exp[Int]]
            val lmsStride = symMirr(stride).asInstanceOf[lms.Exp[Int]]
            val exp = lms.strideArray(lmsXs, lmsStart, lmsLength, lmsStride)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case r@ArrayReplicate(len, value) =>
        createManifest(r.eT) match {
          case mA: Manifest[a_t] =>
            val _len = symMirr(len).asInstanceOf[lms.Exp[Int]]
            val _value = symMirr(value).asInstanceOf[lms.Exp[a_t]]
            val exp = lms.replicate[a_t](_len, _value)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case res@ArrayEmpty() =>
          createManifest(res.eT) match {
            case mA: Manifest[a_t] =>
              val zero = lms.Const(0)
              val exp = lms.array_new[a_t](zero)(mA)
              (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }

      case ArrayAppend(xs, value) =>
        xs.elem match {
          case el: ArrayElem[a] =>
            val mA = createManifest(el.eItem).asInstanceOf[Manifest[a]]
            val lmsXs = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val lmsValue = symMirr(value).asInstanceOf[lms.Exp[a]]
            val exp = lms.array_append(lmsXs, lmsValue)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case lr@ListMap(list, lamSym@Def(lam: Lambda[_, _])) =>
        (createManifest(list.elem), createManifest(lam.eB)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          val lambdaF = mirrorLambdaToLmsFunc[a, b](m)(lam.asInstanceOf[Lambda[a, b]])
          val exp = lms.listMap[a, b](symMirr(list).asInstanceOf[lms.Exp[List[a]]], lambdaF)(mA, mB)
          (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, lambdaF)))
      }

      case lr@ListFilter(list, lamSym @ Def(lam: Lambda[_, _])) =>
        createManifest(list.elem) match {
          case mA: Manifest[a] =>
            val lambdaF = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[Lambda[a, Boolean]])
            val exp = lms.listFilter[a](symMirr(list).asInstanceOf[lms.Exp[List[a]]], lambdaF)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lamSym, lambdaF)))
        }

      case lr@ListRangeFrom0(len) =>
        createManifest(lr.eT) match {
          case mA: Manifest[a] =>
            val exp = lms.listRangeFrom0[a](symMirr(len).asInstanceOf[lms.Exp[Int]])(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case ListHead(xs) =>
        createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] =>
            implicit val imA = mA
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_head[a](ls)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case ListTail(xs) =>
        createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] =>
            implicit val imA = mA
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_tail[a](ls)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case ListCons(x, xs) ⇒
        createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] ⇒
            implicit val imA = mA
            val l = symMirr(x).asInstanceOf[lms.Exp[a]]
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_prepend[a](ls, l)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case ListConcat(xs, ys) =>
        createManifest(xs.elem.eItem) match {
          case mA: Manifest[a] =>
            implicit val imA = mA
            val ls = symMirr(xs).asInstanceOf[lms.Exp[List[a]]]
            val ks = symMirr(ys).asInstanceOf[lms.Exp[List[a]]]
            val exp = lms.list_concat[a](ls, ks)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case ListReplicate(l, x) =>
        createManifest(x.elem) match {
          case mA: Manifest[a] =>
            implicit val imA = mA
            val len = symMirr(l).asInstanceOf[lms.Exp[Int]]
            val el = symMirr(x).asInstanceOf[lms.Exp[a]]
            val exp = lms.list_replicate(len, el)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }
    }
    tt
  }

  def transformMethodCall[T](symMirr: SymMirror, receiver: Exp[_], method: Method, args: List[AnyRef]): lms.Exp[_] =
    !!!("Don't know how to transform method call")
}
