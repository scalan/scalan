package scalan
package compilation.lms

import java.lang.reflect.Method
import java.util.HashMap

import scalan.compilation.language.{CoreMethodMappingDSL, MethodMappingDSL, Interpreter}

trait CoreBridge extends LmsBridge with Interpreter with CoreMethodMappingDSL { self: ScalanCtxExp =>

  val lms: CoreLmsBackendBase

  override def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case _: CompanionBase[_] =>  //TODO backend
      // ignore companion objects
      m

    case mc@MethodCall(receiver, method, args, _) =>
      val isWr = receiver.elem match {
        case el: WrapperElem[_,_] => true
        case el: WrapperElem1[_,_,_,_] => true
        case _ => false
      }

      isWr match {
        case true if method.getName == "wrappedValueOfBaseType" =>
          val _argexp = m.symMirror(receiver)
          m.addSym(sym, _argexp)
        case _ =>
          val exp = transformMethodCall[T](m, receiver, method, args, mc.selfType.asInstanceOf[Elem[T]])
          m.addSym(sym, exp)
      }

    case lr@NewObject(aClass, args, _) =>
      Manifest.classType(aClass) match { //TODO backend: better manifest construction
        case (mA: Manifest[a]) =>
          val exp = newObj[a](m, aClass, args.asInstanceOf[Seq[Rep[_]]], true)(mA)
          m.addSym(sym, exp)
      }

    case lam: Lambda[a, b] =>
      val mA = createManifest(lam.eA).asInstanceOf[Manifest[a]]
      val mB = createManifest(lam.eB).asInstanceOf[Manifest[b]]
      val f = m.mirrorLambda[a, b](lam)
      val fun = lms.fun(f)(mA, mB)
      m.addFuncAndSym(sym, f, fun)

    case Apply(f, x) =>
      (createManifest(f.elem.eDom), createManifest(f.elem.eRange)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          implicit val (imA, imB) = (mA, mB)
          val fun = m.symMirror[a => b](f)
          val arg = m.symMirror[a](x)
          val exp = lms.doApply[a, b](fun, arg)
          m.addSym(sym, exp)
      }

    case c@Const(_) =>
      createManifest(c.selfType) match {
        case mA: Manifest[a] =>
          val x = c.x.asInstanceOf[a]
          val exp = lms.unitD(x)(mA)
          m.addSym(sym, exp)
      }

    case d@Left(l) =>
      (createManifest(d.selfType.eLeft), createManifest(d.selfType.eRight)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          implicit val (imA, imB) = (mA, mB)
          val left = m.symMirror[a](l)
          val exp = lms.make_left[a, b](left)
          m.addSym(sym, exp)
      }

    case d@Right(r) =>
      (createManifest(d.selfType.eLeft), createManifest(d.selfType.eRight)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          implicit val (imA, imB) = (mA, mB)
          val right = m.symMirror[b](r)
          val exp = lms.make_right[a, b](right)
          m.addSym(sym, exp)
      }

    case IsLeft(s) =>
      (createManifest(s.elem.eLeft), createManifest(s.elem.eRight)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          implicit val (imA, imB) = (mA, mB)
          val sum = m.symMirror[Either[a, b]](s)
          val exp = lms.make_isLeft(sum)
          m.addSym(sym, exp)
      }

    case IsRight(s) =>
      (createManifest(s.elem.eLeft), createManifest(s.elem.eRight)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          implicit val (imA, imB) = (mA, mB)
          val sum = m.symMirror[Either[a, b]](s)
          val exp = lms.make_isRight(sum)
          m.addSym(sym, exp)
      }

    case SumFold(s, l, r) =>
      (createManifest(s.elem.eLeft), createManifest(s.elem.eRight), createManifest(l.elem.eRange)) match {
        case (mA: Manifest[a], mB: Manifest[b], mR: Manifest[r]) =>
          implicit val (imA, imB, imR) = (mA, mB, mR)
          val sum = m.symMirror[Either[a, b]](s)
          val left = m.symMirror[a => r](l)
          val right = m.symMirror[b => r](r)
          val exp = lms.make_fold(sum, left, right)
          m.addSym(sym, exp)
      }

    case SumMap(s, l, r) =>
      (createManifest(s.elem.eLeft), createManifest(s.elem.eRight), createManifest(l.elem.eRange), createManifest(r.elem.eRange)) match {
        case (mA: Manifest[a], mB: Manifest[b], mC: Manifest[c], mD: Manifest[d]) =>
          implicit val (imA, imB, imC, imD) = (mA, mB, mC, mD)
          val sum = m.symMirror[Either[a, b]](s)
          val left = m.symMirror[a => c](l)
          val right = m.symMirror[b => d](r)
          val exp = lms.make_map(sum, left, right)
          m.addSym(sym, exp)
      }

    case Tup(fst, snd) =>
      (createManifest(fst.elem), createManifest(snd.elem)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          val first = m.symMirror[a](fst)
          val second = m.symMirror[b](snd)
          val exp = lms.tuple[a, b](first, second)(mA, mB)
          m.addSym(sym, exp)
      }

    case First(tuple) =>
      tuple.elem match {
        case pe: PairElem[_, _] =>
          (createManifest(pe.eFst), createManifest(pe.eSnd)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val tup = m.symMirror[(a, b)](tuple)
              val exp = lms.first[a, b](tup)(mA, mB)
              m.addSym(sym, exp)
          }
      }

    case Second(tuple) =>
      tuple.elem match {
        case pe: PairElem[_, _] =>
          (createManifest(pe.eFst), createManifest(pe.eSnd)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val tup = m.symMirror[(a, b)](tuple)
              val exp = lms.second[a, b](tup)(mA, mB)
              m.addSym(sym, exp)
          }
      }

    case ApplyUnOp(op, arg1) =>
      createManifest(arg1.elem) match {
        case (mA: Manifest[a]) =>
          val arg1_ = m.symMirror[a](arg1)
          val exp: lms.Exp[_] = op.asInstanceOf[UnOp[a, _]] match {
            case Not => lms.Not(arg1_.asInstanceOf[lms.Exp[Boolean]])
            case NumericNegate(n) => lms.Neg(arg1_)(mA, n.asInstanceOf[Numeric[a]])
            case NumericToDouble(n) => mA match {
              case Manifest.Int => lms.IntToDouble(arg1_.asInstanceOf[lms.Exp[Int]])
              case Manifest.Float => lms.FloatToDouble(arg1_.asInstanceOf[lms.Exp[Float]])
              case Manifest.Double => arg1_
            }
            case NumericToFloat(n) => mA match {
              case Manifest.Int => lms.IntToFloat(arg1_.asInstanceOf[lms.Exp[Int]])
              case Manifest.Double => lms.DoubleToFloat(arg1_.asInstanceOf[lms.Exp[Double]])
              case Manifest.Float => arg1_
            }
            case NumericToInt(n) => mA match {
              case Manifest.Float => lms.FloatToInt(arg1_.asInstanceOf[lms.Exp[Float]])
              case Manifest.Double => lms.DoubleToInt(arg1_.asInstanceOf[lms.Exp[Double]])
              case Manifest.Long => lms.LongToIntExt(arg1_.asInstanceOf[lms.Exp[Long]])
              case Manifest.Int => arg1_
            }
            case NumericToString() => lms.ToString(arg1_)
            case HashCode() => lms.hashCode(arg1_)
            case StringToInt() => lms.stringToInt(arg1_.asInstanceOf[lms.Exp[String]])
            case BooleanToInt => lms.booleanToInt(arg1_.asInstanceOf[lms.Exp[Boolean]])
            case StringToDouble() => lms.stringToDouble(arg1_.asInstanceOf[lms.Exp[String]])
            case MathExp => lms.Exp(arg1_.asInstanceOf[lms.Exp[Double]])
            case MathSin => lms.Sin(arg1_.asInstanceOf[lms.Exp[Double]])
            case MathSqrt => lms.Sqrt(arg1_.asInstanceOf[lms.Exp[Double]])
            case MathLog => lms.Log(arg1_.asInstanceOf[lms.Exp[Double]])
            case MathAbs(n) => lms.Abs(arg1_)(mA, n.asInstanceOf[Numeric[a]])
          }
          m.addSym(sym, exp)
      }

    case StringSubstr(str, start, end) =>
      val str_ = m.symMirror[String](str)
      val start_ = m.symMirror[Int](start)
      val end_ = m.symMirror[Int](end)
      val exp = lms.substring(str_, start_, end_)
      m.addSym(sym, exp)

    case StringLength(str) =>
      val str_ = m.symMirror[String](str)
      val exp = lms.stringLength(str_)
      m.addSym(sym, exp)

    case StringApply(str, index) =>
      val str_ = m.symMirror[String](str)
      val index_ = m.symMirror[Int](index)
      val exp = lms.charAt(str_, index_)
      m.addSym(sym, exp)

    case ApplyBinOp(op, arg1, arg2) =>
      createManifest(arg1.elem) match {
        case (mA: Manifest[a]) =>
          val arg1_ = m.symMirror[a](arg1)
          val arg2_ = m.symMirror[a](arg2)
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
          m.addSym(sym, exp)
      }

    case ThrowException(msg) =>
      val msg_ = m.symMirror[String](msg)
      val exp = lms.throwException(msg_)
      m.addSym(sym, exp)

    case Semicolon(left, right) =>
      (createManifest(left.elem), createManifest(right.elem)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          val left_ = m.symMirror[a](left)
          val right_ = m.symMirror[b](right)
          val exp = lms.block(left_, right_)(mA, mB)
          m.addSym(sym, exp)
      }

    case i@IfThenElse(cond, ifTrue, ifFalse) =>
      createManifest(i.selfType) match {
        case (mA: Manifest[a]) =>
          val cond_ = m.symMirror[Boolean](cond)

          g.branches.ifBranches.get(sym) match {
            case Some(branches) =>
              def thenBody = m.mirrorBlock(branches.thenBody, ifTrue)
              def elseBody = m.mirrorBlock(branches.elseBody, ifFalse)
              val exp = lms.ifThenElse(cond_, thenBody, elseBody)(mA)
              m.addSym(sym, exp)
            case _ =>
              val then_ = m.symMirror[a](ifTrue)
              val else_ = m.symMirror[a](ifFalse)
              val exp = lms.ifThenElse(cond_, () => then_, () => else_)(mA)
              m.addSym(sym, exp)
          }
      }

    case loop@LoopUntil(state, stepSym@Def(step: Lambda[_, _]), condSym@Def(cond: Lambda[_, _])) =>
      createManifest(loop.selfType) match {
        case (mA: Manifest[a]) =>
          val cond_ = m.mirrorLambda[a, Boolean](cond.asInstanceOf[Lambda[a, Boolean]])
          val step_ = m.mirrorLambda[a, a](step.asInstanceOf[Lambda[a, a]])
          val state_ = m.symMirror[a](state)
          val exp = lms.loopUntil[a](state_, cond_, step_)(mA)
          m.addSym(sym, exp).addFunc(condSym, cond_).addFunc(stepSym, step_)
      }
    //
    // Array Buffer
    //
    case buf@ArrayBufferEmpty() =>
      buf.selfType match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val exp = lms.emptyArrayBuffer[t]()(mT)
              m.addSym(sym, exp)
          }
      }
    case buf@MakeArrayBuffer(ctx) =>
      buf.selfType match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val exp = lms.emptyArrayBuffer[t]()(mT)
              m.addSym(sym, exp)
          }
      }
    case buf@ArrayBufferFromElem(e) =>
      buf.selfType match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val v = m.symMirror[t](e)
              val exp = lms.arrayBufferFromElem(v)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferApply(buf, i) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val i_ = m.symMirror[Int](i)
              val exp = lms.arrayBufferApply(b_, i_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferLength(buf) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val exp = lms.arrayBufferLength(b_)(mT)
              m.addSym(sym, exp)
          }
      }
    case res@ArrayBufferMap(buf, lambdaSym@Def(lambda: Lambda[_, _])) =>
      (res.selfType, buf.elem) match {
        case (from: ArrayBufferElem[a], to: ArrayBufferElem[b]) =>
          (createManifest(from.eItem), createManifest(to.eItem)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[a]](buf)
              val f_ = m.mirrorLambda[a, b](lambda.asInstanceOf[Lambda[a, b]])
              val exp = lms.arrayBufferMap(b_, f_)(mA, mB)
              m.addSym(sym, exp).addFunc(lambdaSym, f_)
          }
      }
    case ArrayBufferUpdate(buf, i, v) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val i_ = m.symMirror[Int](i)
              val v_ = m.symMirror[t](v)
              val exp = lms.arrayBufferUpdate(b_, i_, v_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferInsert(buf, i, v) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val i_ = m.symMirror[Int](i)
              val v_ = m.symMirror[t](v)
              val exp = lms.arrayBufferInsert(b_, i_, v_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferAppend(buf, v) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val v_ = m.symMirror[t](v)
              val exp = lms.arrayBufferAppend(b_, v_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferAppendArray(buf, a) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val a_ = m.symMirror[Array[t]](a)
              val exp = lms.arrayBufferAppendArray(b_, a_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferRemove(buf, i, n) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val i_ = m.symMirror[Int](i)
              val n_ = m.symMirror[Int](n)
              val exp = lms.arrayBufferRemove(b_, i_, n_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferReset(buf) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val exp = lms.arrayBufferReset(b_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ArrayBufferToArray(buf) =>
      buf.elem match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val b_ = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
              val exp = lms.arrayBufferToArray(b_)(mT)
              m.addSym(sym, exp)
          }
      }
    case ab@ArrayBufferRep(buf) =>
      ab.selfType match {
        case elem: ArrayBufferElem[t] =>
          val exp = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
          m.addSym(sym, exp)
      }
    case buf@ArrayBufferUsingFunc(count, lambdaSym@Def(lambda: Lambda[_, _])) =>
      buf.selfType match {
        case elem: ArrayBufferElem[t] =>
          createManifest(elem.eItem) match {
            case mT: Manifest[t] =>
              val n = m.symMirror[Int](count)
              val f = m.mirrorLambda[Int, t](lambda.asInstanceOf[Lambda[Int, t]])
              val exp = lms.arrayBufferUsingFunc(n, f)(mT)
              m.addSym(sym, exp).addFunc(lambdaSym, f)
          }
      }

    //
    // Map
    //
    case AppendMultiMap(map, key, value) =>
      (key.elem, value.elem) match {
        case (eK: Elem[k], eV: Elem[v]) =>
          (createManifest(eK), createManifest(eV)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, scala.collection.mutable.ArrayBuilder[v]]](map)
              val key_ = m.symMirror[k](key)
              val value_ = m.symMirror[v](value)
              val exp = lms.multiMapAppend(map_, key_, value_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case pm@VarMM(map) =>
      pm.selfType match {
        case elem: MMapElem[k, v] =>
          val exp = m.symMirror[MMap[k, v]](map)
          m.addSym(sym, exp)
      }

    case map@MapFromArray(arr) =>
      map.selfType match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val arr_ = m.symMirror[Array[(k, v)]](arr)
              val exp = lms.mapFromArray[k, v](arr_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case map@MapUsingFunc(count, lambdaSym@Def(lambda: Lambda[_, _])) =>
      map.selfType match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val n = m.symMirror[Int](count)
              val f = m.mirrorLambda[Int, (k, v)](lambda.asInstanceOf[Lambda[Int, (k, v)]])
              val exp = lms.mapUsingFunc[k, v](n, f)(mK, mV)
              m.addSym(sym, exp).addFunc(lambdaSym, f)
          }
      }
    case map@EmptyMap() =>
      map.selfType match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val exp = lms.emptyMap[k, v]()(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case map@MakeMap(ctx) =>
      map.selfType match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val exp = lms.emptyMap[k, v]()(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case MapUnion(left, right) =>
      left.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val left_ = m.symMirror[HashMap[k, v]](left)
              val right_ = m.symMirror[HashMap[k, v]](right)
              val exp = lms.mapUnion[k, v](left_, right_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case MapDifference(left, right) =>
      left.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val left_ = m.symMirror[HashMap[k, v]](left)
              val right_ = m.symMirror[HashMap[k, v]](right)
              val exp = lms.mapDifference[k, v](left_, right_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case tk@MapTransformValues(map, lamSym@Def(lambda: Lambda[_, _])) =>
      (map.elem, tk.selfType) match {
        case (in: MMapElem[k, v], out: MMapElem[_, t]) =>
          (createManifest(in.eKey), createManifest(in.eValue), createManifest(out.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v], mT: Manifest[t]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val f = m.mirrorLambda[v, t](lambda.asInstanceOf[Lambda[v, t]])
              val exp = lms.mapTransformValues[k, v, t](map_, f)(mK, mV, mT)
              m.addSym(sym, exp).addFunc(lamSym, f)
          }
      }
    case MapReduce(left, right, reduceSym@Def(reduce: Lambda[_, _])) =>
      left.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val left_ = m.symMirror[HashMap[k, v]](left)
              val right_ = m.symMirror[HashMap[k, v]](right)
              val reduce_ = m.mirrorLambda[(v, v), v](reduce.asInstanceOf[Lambda[(v, v), v]])
              val exp = lms.mapReduce[k, v](left_, right_, reduce_)(mK, mV)
              m.addSym(sym, exp).addFunc(reduceSym, reduce_)
          }
      }
    case MapJoin(left, right) =>
      (left.elem, right.elem) match {
        case (e1: MMapElem[k1, v1], e2: MMapElem[k2, v2]) =>
          (createManifest(e1.eKey), createManifest(e1.eValue), createManifest(e2.eValue)) match {
            case (mK: Manifest[k1], mV1: Manifest[v1], mV2: Manifest[v2]) =>
              val left_ = m.symMirror[HashMap[k1, v1]](left)
              val right_ = m.symMirror[HashMap[k1, v2]](right)
              val exp = lms.mapJoin[k1, v1, v2](left_, right_)(mK, mV1, mV2)
              m.addSym(sym, exp)
          }
      }
    case MapContains(map, key) =>
      map.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val key_ = m.symMirror[k](key)
              val exp = lms.mapContains[k, v](map_, key_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case MapApply(map, key) =>
      map.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val key_ = m.symMirror[k](key)
              val exp = lms.mapApply[k, v](map_, key_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case maf@MapApplyIf(map, key, s1@Def(exists: Lambda[_, _]), s2@Def(otherwise: Lambda[_, _])) =>
      (maf.selfType, map.elem) match {
        case (eT: Elem[t], eM: MMapElem[k, v]) =>
          (createManifest(eT), createManifest(eM.eKey), createManifest(eM.eValue)) match {
            case (mT: Manifest[t], mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val key_ = m.symMirror[k](key)
              val exists_ = m.mirrorLambda[v, t](exists.asInstanceOf[Lambda[v, t]])
              val otherwise_ = m.mirrorLambda[Unit, t](otherwise.asInstanceOf[Lambda[Unit, t]])
              val exp = lms.mapApplyIf[k, v, t](map_, key_, exists_, otherwise_)(mK, mV, mT)
              m.addSym(sym, exp).addFunc(s1, exists_).addFunc(s2, otherwise_)
          }
      }
    case MapUpdate(map, key, value) =>
      map.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val key_ = m.symMirror[k](key)
              val value_ = m.symMirror[v](value)
              val exp = lms.mapUpdate[k, v](map_, key_, value_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case MapSize(map) =>
      map.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val exp = lms.mapSize[k, v](map_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case MapToArray(map) =>
      map.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val exp = lms.mapToArray[k, v](map_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case MapKeys(map) =>
      map.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val exp = lms.mapKeys[k, v](map_)(mK, mV)
              m.addSym(sym, exp)
          }
      }
    case MapValues(map) =>
      map.elem match {
        case elem: MMapElem[k, v] =>
          (createManifest(elem.eKey), createManifest(elem.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val map_ = m.symMirror[HashMap[k, v]](map)
              val exp = lms.mapValues[k, v](map_)(mK, mV)
              m.addSym(sym, exp)
          }
      }

    case apply@ArrayApply(xs, ind) =>
      createManifest(apply.selfType) match {
        case (mA: Manifest[a]) =>
          val xs_ = m.symMirror[Array[a]](xs)
          val ind_ = m.symMirror[Int](ind)
          val exp = lms.arrayGet[a](xs_, ind_)(mA)
          m.addSym(sym, exp)
      }

    case ArrayApplyMany(xs, idxs) =>
      xs.elem match {
        case el: ArrayElem[_] =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val xs_ = m.symMirror[Array[a]](xs)
              val idxs_ = m.symMirror[Array[Int]](idxs)
              val exp = lms.arrayGather[a](xs_, idxs_)(mA)
              m.addSym(sym, exp)
          }
      }

    case ArrayLength(xs) =>
      createManifest(xs.elem) match {
        case mA: Manifest[a] =>
          val xs_ = m.symMirror[Array[a]](xs)
          val exp = lms.arrayLength[a](xs_)(mA)
          m.addSym(sym, exp)
      }

    case ArrayRangeFrom0(n) =>
      val n_ = m.symMirror[Int](n)
      val exp = lms.indexRangeArray(n_)
      m.addSym(sym, exp)

    case ArraySort(arg, o) =>
      arg.elem match {
        case (el: ArrayElem[_]) =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val arg_ = m.symMirror[Array[a]](arg)
              val exp = lms.arraySort[a](arg_)(mA)
              m.addSym(sym, exp)
          }
      }
    case ArrayReverse(arg) =>
      arg.elem match {
        case (el: ArrayElem[_]) =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val arg_ = m.symMirror[Array[a]](arg)
              val exp = lms.arrayReverse[a](arg_)(mA)
              m.addSym(sym, exp)
          }
      }
    case sort@ArraySortBy(arg, lambdaSym@Def(by: Lambda[_, b]), o) =>
      sort.selfType match {
        case (el: ArrayElem[a]) =>
          (createManifest(el.eItem), createManifest(by.eB)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val by_ = m.mirrorLambda[a, b](by.asInstanceOf[Lambda[a, b]])
              val arg_ = m.symMirror[Array[a]](arg)
              val exp = lms.arraySortBy[a, b](arg_, by_)(mA, mB)
              m.addSym(sym, exp).addFunc(lambdaSym, by_)
          }
      }
    case gby@ArrayGroupBy(arg, lambdaSym@Def(by: Lambda[_, _])) =>
      (arg.elem, gby.selfType) match {
        case (ae: ArrayElem[a], me: MMapElem[k, v]) =>
          (createManifest(ae.eItem), createManifest(me.eKey)) match {
            case (mA: Manifest[a], mK: Manifest[k]) =>
              val by_ = m.mirrorLambda[a, k](by.asInstanceOf[Lambda[a, k]])
              val arg_ = m.symMirror[Array[a]](arg)
              val exp = lms.arrayGroupBy[a, k](arg_, by_)(mA, mK)
              m.addSym(sym, exp).addFunc(lambdaSym, by_)
          }
      }
    case sum@ArraySum(xs, n) =>
      createManifest(sum.selfType) match {
        case (mA: Manifest[a]) =>
          val xs_ = m.symMirror[Array[a]](xs)
          val exp = lms.arraySum[a](xs_)(mA, n.asInstanceOf[Numeric[a]])
          m.addSym(sym, exp)
      }
    case min@ArrayMin(xs, o) =>
      createManifest(min.selfType) match {
        case (mA: Manifest[a]) =>
          val xs_ = m.symMirror[Array[a]](xs)
          val exp = lms.arrayMin[a](xs_)(mA, o.asInstanceOf[Ordering[a]])
          m.addSym(sym, exp)
      }
    case max@ArrayMax(xs, o) =>
      createManifest(max.selfType) match {
        case (mA: Manifest[a]) =>
          val xs_ = m.symMirror[Array[a]](xs)
          val exp = lms.arrayMax[a](xs_)(mA, o.asInstanceOf[Ordering[a]])
          m.addSym(sym, exp)
      }
    case ArrayAvg(xs, n) =>
      xs.elem match {
        case (el: ArrayElem[a]) =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val xs_ = m.symMirror[Array[a]](xs)
              val exp = lms.arrayAvg[a](xs_)(mA, n.asInstanceOf[Numeric[a]])
              m.addSym(sym, exp)
          }
      }

    case ArrayZip(arg1, arg2) =>
      (arg1.elem, arg2.elem) match {
        case (el1: ArrayElem[_], el2: ArrayElem[_]) =>
          (createManifest(el1.eItem), createManifest(el2.eItem)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val arg1_ = m.symMirror[Array[a]](arg1)
              val arg2_ = m.symMirror[Array[b]](arg2)
              val exp = lms.arrayZip[a, b](arg1_, arg2_)(mA, mB)
              m.addSym(sym, exp)
          }
      }

    case map@ArrayMap(source, lambdaSym@Def(lam: Lambda[_, _])) =>
      (source.elem, map.selfType) match {
        case (el: ArrayElem[_], el1: ArrayElem[_]) =>
          (createManifest(el.eItem), createManifest(el1.eItem)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val f = m.mirrorLambda[a, b](lam.asInstanceOf[Lambda[a, b]]) //(mA, mB)
            val lmsSource = m.symMirror[Array[a]](source)
              val exp = lms.mapArray[a, b](lmsSource, f)(mA, mB)
              m.addSym(sym, exp).addFunc(lambdaSym, f)
          }
      }

    case map@ArrayFlatMap(source, lambdaSym@Def(lam: Lambda[_, _])) =>
      (source.elem, map.selfType) match {
        case (el: ArrayElem[_], el1: ArrayElem[_]) =>
          (createManifest(el.eItem), createManifest(el1.eItem)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val f = m.mirrorLambda[a, Array[b]](lam.asInstanceOf[Lambda[a, Array[b]]]) //(mA, mB)
              val lmsSource = m.symMirror[Array[a]](source)
              val exp = lms.flatMapArray[a, b](lmsSource, f)(mA, mB)
              m.addSym(sym, exp).addFunc(lambdaSym, f)
          }
      }

    case filter@ArrayFilter(source, lambdaSym@Def(lam: Lambda[_, _])) =>
      filter.selfType match {
        case el: ArrayElem[_] =>
          createManifest(el.eItem) match {
            case mA: Manifest[a] =>
              val f = m.mirrorLambda[a, Boolean](lam.asInstanceOf[Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = m.symMirror[Array[a]](source)
              val exp = lms.filterArray[a](lmsSource, f)(mA)
              m.addSym(sym, exp).addFunc(lambdaSym, f)
          }
      }

    case find@ArrayFind(source, lambdaSym@Def(lam: Lambda[_, _])) =>
      (source.elem) match {
        case (el: ArrayElem[_]) =>
          (createManifest(el.eItem)) match {
            case (mA: Manifest[a]) =>
              val f = m.mirrorLambda[a, Boolean](lam.asInstanceOf[Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = m.symMirror[Array[a]](source)
              val exp = lms.findArray[a](lmsSource, f)(mA)
              m.addSym(sym, exp).addFunc(lambdaSym, f)
          }
      }
    case ArrayCount(source, lambdaSym@Def(lam: Lambda[_, _])) =>
      (source.elem) match {
        case (el: ArrayElem[_]) =>
          (createManifest(el.eItem)) match {
            case (mA: Manifest[a]) =>
              val f = m.mirrorLambda[a, Boolean](lam.asInstanceOf[Lambda[a, Boolean]]) //(mA, mB)
              val lmsSource = m.symMirror[Array[a]](source)
              val exp = lms.countArray[a](lmsSource, f)(mA)
              m.addSym(sym, exp).addFunc(lambdaSym, f)
          }
      }
    case mr@ArrayMapReduce(Def(range: ArrayRangeFrom0), mapSym@Def(map: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) =>
      (mr.selfType) match {
        case (me: MMapElem[k, v]) =>
          (createManifest(me.eKey), createManifest(me.eValue)) match {
            case (mK: Manifest[k], mV: Manifest[v]) =>
              val n_ = m.symMirror[Int](range.n)
              val map_ = m.mirrorLambda[Int, (k, v)](map.asInstanceOf[Lambda[Int, (k, v)]])
              val reduce_ = m.mirrorLambda[(v, v), v](reduce.asInstanceOf[Lambda[(v, v), v]])
              val exp = lms.rangeMapReduce(n_, map_, reduce_)(mK, mV)
              m.addSym(sym, exp).addFunc(mapSym, map_).addFunc(reduceSym, reduce_)
          }
      }
    case mr@ArrayMapReduce(Def(ArrayFilter(Def(ArrayMap(Def(range: ArrayRangeFrom0), map1Sym@Def(map1: Lambda[_, _]))), filterSym@Def(filter: Lambda[_, _]))),
    map2Sym@Def(map2: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) =>
      (map1.eB, mr.selfType) match {
        case (ma: Elem[a], me: MMapElem[k, v]) =>
          (createManifest(ma), createManifest(me.eKey), createManifest(me.eValue)) match {
            case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) =>
              val n_ = m.symMirror[Int](range.n)
              val map1_ = m.mirrorLambda[Int, a](map1.asInstanceOf[Lambda[Int, a]])
              val filter_ = m.mirrorLambda[a, Boolean](filter.asInstanceOf[Lambda[a, Boolean]])
              val map2_ = m.mirrorLambda[a, (k, v)](map2.asInstanceOf[Lambda[a, (k, v)]])
              val reduce_ = m.mirrorLambda[(v, v), v](reduce.asInstanceOf[Lambda[(v, v), v]])
              val exp = lms.rangeFilterMapReduce(n_, map1_, filter_, map2_, reduce_)(mA, mK, mV)
              m.addSym(sym, exp).addFunc(map1Sym, map1_).addFunc(map2Sym, map2_).
                addFunc(filterSym, filter_).addFunc(reduceSym, reduce_)
          }
      }
    case mr@ArrayMapReduce(Def(ArrayFilter(source, filterSym@Def(filter: Lambda[_, _]))), mapSym@Def(map: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) =>
      (source.elem, mr.selfType) match {
        case (ae: ArrayElem[a], me: MMapElem[k, v]) =>
          (createManifest(ae.eItem), createManifest(me.eKey), createManifest(me.eValue)) match {
            case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) =>
              val source_ = m.symMirror[Array[a]](source)
              val filter_ = m.mirrorLambda[a, Boolean](filter.asInstanceOf[Lambda[a, Boolean]])
              val map_ = m.mirrorLambda[a, (k, v)](map.asInstanceOf[Lambda[a, (k, v)]])
              val reduce_ = m.mirrorLambda[(v, v), v](reduce.asInstanceOf[Lambda[(v, v), v]])
              val exp = lms.filterMapReduce(source_, filter_, map_, reduce_)(mA, mK, mV)
              m.addSym(sym, exp).addFunc(mapSym, map_).
                addFunc(filterSym, filter_).addFunc(reduceSym, reduce_)
          }
      }
    case mr@ArrayMapReduce(source, mapSym@Def(map: Lambda[_, _]), reduceSym@Def(reduce: Lambda[_, _])) =>
      (source.elem, mr.selfType) match {
        case (ae: ArrayElem[a], me: MMapElem[k, v]) =>
          (createManifest(ae.eItem), createManifest(me.eKey), createManifest(me.eValue)) match {
            case (mA: Manifest[a], mK: Manifest[k], mV: Manifest[v]) =>
              val source_ = m.symMirror[Array[a]](source)
              val map_ = m.mirrorLambda[a, (k, v)](map.asInstanceOf[Lambda[a, (k, v)]])
              val reduce_ = m.mirrorLambda[(v, v), v](reduce.asInstanceOf[Lambda[(v, v), v]])
              val exp = lms.arrayMapReduce(source_, map_, reduce_)(mA, mK, mV)
              m.addSym(sym, exp).addFunc(mapSym, map_).addFunc(reduceSym, reduce_)
          }
      }
    case f@ArrayFold(source, init, stepSym@Def(step: Lambda[_, _])) =>
      (f.selfType, source.elem) match {
        case (e: Elem[s], ae: ArrayElem[a]) =>
          (createManifest(e), createManifest(ae.eItem)) match {
            case (mS: Manifest[s], mA: Manifest[a]) =>
              val src = m.symMirror[Array[a]](source)
              val state = m.symMirror[s](init)
              val func = m.mirrorLambda[(s, a), s](step.asInstanceOf[Lambda[(s, a), s]])
              val exp = lms.foldArray[a, s](src, state, func)(mA, mS)
              m.addSym(sym, exp).addFunc(stepSym, func)
          }
      }
    case sum@ArraySumBy(source, lamSym@Def(f: Lambda[_, _]), n) =>
      (sum.selfType, source.elem) match {
        case (e: Elem[s], ae: ArrayElem[a]) =>
          (createManifest(e), createManifest(ae.eItem)) match {
            case (mS: Manifest[s], mA: Manifest[a]) =>
              val src = m.symMirror[Array[a]](source)
              val func = m.mirrorLambda[a, s](f.asInstanceOf[Lambda[a, s]])
              val exp = lms.sumArrayBy[a, s](src, func)(mA, mS, n.asInstanceOf[Numeric[s]])
              m.addSym(sym, exp).addFunc(lamSym, func)
          }
      }
    case ParallelExecute(nJobs, lamSym@Def(f: Lambda[_, _])) =>
      f.eB match {
        case el: Elem[b] =>
          createManifest(el) match {
            case (mB: Manifest[b]) =>
              val n = m.symMirror[Int](nJobs)
              val func = m.mirrorLambda[Int, b](f.asInstanceOf[Lambda[Int, b]])
              val exp = lms.parallelExecute[b](n, func)(mB)
              m.addSym(sym, exp).addFunc(lamSym, func)
          }
      }

    case ArrayUpdate(xs, index, value) =>
      xs.elem match {
        case el: ArrayElem[a] =>
          val mA = createManifest(el.eItem).asInstanceOf[Manifest[a]]
          val lmsXs = m.symMirror[Array[a]](xs)
          val lmsIndex = m.symMirror[Int](index)
          val lmsValue = m.symMirror[a](value)
          val exp = lms.updateArray(lmsXs, lmsIndex, lmsValue)(mA)
          m.addSym(sym, exp)
      }

    case ArrayReduce(source, monoid) =>
      source.elem match {
        case el: ArrayElem[_] =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val src = m.symMirror[Array[a]](source)
              monoid.opName match {
                case "+" =>
                  val exp = lms.sumArray[a](src)(mA)
                  m.addSym(sym, exp)
                case _ =>
                  monoid.append match {
                    case opSym@Def(lambda: Lambda[_, _]) =>
                      val zero = m.symMirror[a](monoid.zero)
                      val op = m.mirrorLambda[(a, a), a](lambda.asInstanceOf[Lambda[(a, a), a]])
                      val exp = lms.reduceArray[a](src, zero, op)(mA)
                      m.addSym(sym, exp).addFunc(opSym, op)
                  }
              }
          }
      }

    case ArrayScan(source, monoid) =>
      source.elem match {
        case el: ArrayElem[_] =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val src = m.symMirror[Array[a]](source)
              monoid.opName match {
                //case "+" =>
                //  val exp = lms.sum[a](src)(mA)
                //  m.addSym(sym, exp)
                case _ =>
                  monoid.append match {
                    case opSym@Def(lambda: Lambda[_, _]) =>
                      val zero = m.symMirror[a](monoid.zero)
                      val op = m.mirrorLambda[(a, a), a](lambda.asInstanceOf[Lambda[(a, a), a]])
                      val exp = lms.scanArray[a](src, zero, op)(mA)
                      m.addSym(sym, exp).addFunc(opSym, op)
                  }
              }
          }
      }

    case ArrayStride(xs, start, length, stride) =>
      xs.elem match {
        case el: ArrayElem[a] =>
          val mA = createManifest(el.eItem).asInstanceOf[Manifest[a]]
          val lmsXs = m.symMirror[Array[a]](xs)
          val lmsStart = m.symMirror[Int](start)
          val lmsLength = m.symMirror[Int](length)
          val lmsStride = m.symMirror[Int](stride)
          val exp = lms.strideArray(lmsXs, lmsStart, lmsLength, lmsStride)(mA)
          m.addSym(sym, exp)
      }

    case r@ArrayReplicate(len, value) =>
      createManifest(r.eT) match {
        case mA: Manifest[a_t] =>
          val _len = m.symMirror[Int](len)
          val _value = m.symMirror[a_t](value)
          val exp = lms.arrayReplicate[a_t](_len, _value)(mA)
          m.addSym(sym, exp)
      }

    case res@ArrayEmpty() =>
        createManifest(res.eT) match {
          case mA: Manifest[a_t] =>
            val zero = lms.Const(0)
            val exp = lms.array_new[a_t](zero)(mA)
            m.addSym(sym, exp)
        }

    case ArrayAppend(xs, value) =>
      xs.elem match {
        case el: ArrayElem[a] =>
          val mA = createManifest(el.eItem).asInstanceOf[Manifest[a]]
          val lmsXs = m.symMirror[Array[a]](xs)
          val lmsValue = m.symMirror[a](value)
          val exp = lms.array_append(lmsXs, lmsValue)(mA)
          m.addSym(sym, exp)
      }

    case ArrayCons(value, xs) =>
      xs.elem match {
        case el: ArrayElem[a] =>
          val mA = createManifest(el.eItem).asInstanceOf[Manifest[a]]
          val lmsXs = m.symMirror[Array[a]](xs)
          val lmsValue = m.symMirror[a](value)
          val exp = lms.array_cons(lmsValue, lmsXs)(mA)
          m.addSym(sym, exp)
      }

    case ArrayToList(xs) =>
      createManifest(xs.elem.eItem) match {
        case mA: Manifest[a] =>
          val lmsXs = m.symMirror[Array[a]](xs)
          val exp = lms.arrayToList(lmsXs)(mA)
          m.addSym(sym, exp)
      }

    case lr@ListMap(list, lamSym@Def(lam: Lambda[_, _])) =>
      (createManifest(list.elem.eItem), createManifest(lam.eB)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val lambdaF = m.mirrorLambda[a, b](lam.asInstanceOf[Lambda[a, b]])
        val exp = lms.listMap[a, b](m.symMirror[List[a]](list), lambdaF)(mA, mB)
        m.addSym(sym, exp).addFunc(lamSym, lambdaF)
    }

    case lr@ListFlatMap(list, lamSym@Def(lam: Lambda[_, _])) =>
      lam.eB match {
        case el: ListElem[_] =>
          (createManifest(list.elem.eItem), createManifest(el.eItem)) match {
            case (mA: Manifest[a], mB: Manifest[b]) =>
              val lambdaF = m.mirrorLambda[a, List[b]](lam.asInstanceOf[Lambda[a, List[b]]])
              val exp = lms.listFlatMap[a, b](m.symMirror[List[a]](list), lambdaF)(mA, mB)
              m.addSym(sym, exp).addFunc(lamSym, lambdaF)
          }
      }

//    case lr@ListFoldLeft(list: Lst[a], init: Exp[s], lamSym @ Def(lam: Lambda[_, _])) =>
//      (createManifest(list.elem.eItem), createManifest(init.elem)) match {
//        case (mA: Manifest[a], mS: Manifest[s]) =>
//          val lambdaF = m.mirrorLambda[(s,a), s](lam.asInstanceOf[Lambda[(s,a), s]])
//          val exp = lms.listFlatMap[a, b](m.symMirror[List[a]](list), lambdaF)(mA, mB)
//          m.addSym(sym, exp).addFunc(lamSym, lambdaF)
//      }

    case lr@ListLength(list) =>
      createManifest(list.elem.eItem) match {
        case (mA: Manifest[a]) =>
          val exp = lms.listLength[a](m.symMirror[List[a]](list))(mA)
          m.addSym(sym, exp)
      }

    case lr@ListFilter(list, lamSym @ Def(lam: Lambda[_, _])) =>
      createManifest(list.elem.eItem) match {
        case mA: Manifest[a] =>
          val lambdaF = m.mirrorLambda[a, Boolean](lam.asInstanceOf[Lambda[a, Boolean]])
          val exp = lms.listFilter[a](m.symMirror[List[a]](list), lambdaF)(mA)
          m.addSym(sym, exp).addFunc(lamSym, lambdaF)
      }

    case lr@ListRangeFrom0(len) =>
      createManifest(lr.eT) match {
        case mA: Manifest[a] =>
          val exp = lms.listRangeFrom0[a](m.symMirror[Int](len))(mA)
          m.addSym(sym, exp)
      }

    case ListHead(xs) =>
      createManifest(xs.elem.eItem) match {
        case mA: Manifest[a] =>
          implicit val imA = mA
          val ls = m.symMirror[List[a]](xs)
          val exp = lms.list_head[a](ls)
          m.addSym(sym, exp)
      }

    case ListTail(xs) =>
      createManifest(xs.elem.eItem) match {
        case mA: Manifest[a] =>
          implicit val imA = mA
          val ls = m.symMirror[List[a]](xs)
          val exp = lms.list_tail[a](ls)
          m.addSym(sym, exp)
      }

    case ListCons(x, xs) ⇒
      createManifest(xs.elem.eItem) match {
        case mA: Manifest[a] ⇒
          implicit val imA = mA
          val l = m.symMirror[a](x)
          val ls = m.symMirror[List[a]](xs)
          val exp = lms.list_prepend[a](ls, l)
          m.addSym(sym, exp)
      }

    case ListConcat(xs, ys) =>
      createManifest(xs.elem.eItem) match {
        case mA: Manifest[a] =>
          implicit val imA = mA
          val ls = m.symMirror[List[a]](xs)
          val ks = m.symMirror[List[a]](ys)
          val exp = lms.list_concat[a](ls, ks)
          m.addSym(sym, exp)
      }
    case ListToArray(xs) =>
      createManifest(xs.elem.eItem) match {
        case mA: Manifest[a] =>
          implicit val imA = mA
          val ls = m.symMirror[List[a]](xs)
          val exp = lms.list_toarray[a](ls)
          m.addSym(sym, exp)
      }

    case ListReplicate(l, x) =>
      createManifest(x.elem) match {
        case mA: Manifest[a] =>
          implicit val imA = mA
          val len = m.symMirror[Int](l)
          val el = m.symMirror[a](x)
          val exp = lms.list_replicate(len, el)
          m.addSym(sym, exp)
      }

    case ListReduce(source, monoid) =>
      createManifest(monoid.eA) match {
        case (mA: Manifest[a]) =>
          val src = m.symMirror[List[a]](source)
          // may want to special-case e.g. sum and product if sumList can be implemented generically
          // (see comment there and implementation for ArrayReduce above)
          monoid.append match {
            case opSym@Def(lambda: Lambda[_, _]) =>
              val zero = monoid.zero
              val lmsZero = m.symMirror[a](zero)
              val op = m.mirrorLambda[(a, a), a](lambda.asInstanceOf[Lambda[(a, a), a]])
              val exp = lms.reduceList[a](src, lmsZero, op)(mA)
              m.addSym(zero, lmsZero).addSym(sym, exp).addFunc(opSym, op)
          }
      }

    case ArrayBinarySearch(i, xs, o) =>
        xs.elem match {
          case el: ArrayElem[_] =>
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val idxs = m.symMirror[Array[Int]](xs)
                val index = m.symMirror[Int](i)
                val exp = lms.array_binarySearch[a](index, idxs)(mA)
                m.addSym(sym, exp)
            }
        }

    case ArrayRandomGaussian(a, e, xs) =>
        xs.elem match {
          case el: ArrayElem[_] =>
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val array = m.symMirror[Array[Double]](xs)
                val median = m.symMirror[Double](a)
                val dev = m.symMirror[Double](e)
                val exp = lms.array_randomGaussian[a](median, dev, array)(mA)
                m.addSym(sym, exp)
            }
        }

    case _ => super.transformDef(m, g, sym, d)
  }

  def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] =
    !!!(s"Don't know how to transform method call: $method")
  def newObj[A: Manifest](m: LmsMirror, aClass: Class[_], args: Seq[Rep[_]], newKeyWord: Boolean): lms.Exp[A] = !!!("Don't know how to create new object")
}
