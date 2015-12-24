package scalan
package compilation.lms

import java.lang.reflect.Method
import scala.reflect.SourceContext
import scala.reflect.runtime.universe._

import scalan.util.{ParamMirror, StringUtil}

trait CoreBridge extends StructBridge {
  import scalan._

  val lms: CoreLmsBackend

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: Const[_] => "unitD"
    case _: Apply[_, _] => "doApply"
    case _: SLeft[_, _] => "either_left"
    case _: SRight[_, _] => "either_right"
    case _: SumFold[_, _, _] => "either_fold"
    case _: SumMap[_, _, _, _] => "either_map"
    case _: Tup[_, _] => "tuple2"
    case _: First[_, _] => "tuple2_get1"
    case _: Second[_, _] => "tuple2_get2"
    case _: ArrayBufferEmpty[_] => "arraybuilder_make"
    case _: MakeArrayBuffer[_] => "arraybuilder_make"
    case _: ArrayBufferToArray[_] => "arraybuilder_result"
    case _: MakeMap[_, _] => "hashmap_new"
    case _: ListToArray[_] => "list_toarray"
    case _: EmptyMap[_, _] => "hashmap_new"
    case _: AppendMultiMap[_, _] => "multiMap_append"
    case _: MapContains[_, _] => "hashmap_contains"
    case _: MapApply[_, _] => "hashmap_apply"
    case _: MapSize[_, _] => "hashmap_size"
    // covered by the below case
//    case s if s.startsWith("ArrayBuffer") =>
//      StringUtil.lowerCaseFirst(s)
    // See comment in ArrayOpsExt
    case _ if primitiveName.startsWith("Array") =>
      StringUtil.lowerCaseFirst(primitiveName)
    case _ => super.lmsMethodName(d, primitiveName)
  }

  override protected def extractParams(d: Def[_], paramMirrors: List[ParamMirror]) = d match {
    case Apply(f, x) =>
      List(f, x, x.elem, f.elem.eRange)
    case d@SLeft(l) =>
      List(l, d.selfType.eLeft, d.selfType.eRight)
    case d@SRight(r) =>
      List(r, d.selfType.eLeft, d.selfType.eRight)
    case SumFold(s, l, r) =>
      List(s, l, r, s.elem.eLeft, s.elem.eRight, l.elem.eRange)
    case SumMap(s, l, r) =>
      List(s, l, r, s.elem.eLeft, s.elem.eRight, l.elem.eRange, r.elem.eRange)
    case First(pair) =>
      List(pair, pair.elem.eFst)
    case Second(pair) =>
      List(pair, pair.elem.eSnd)
    case ArrayMap(xs, f) =>
      List(xs, f, xs.elem.eItem, f.elem.eRange)
    case ArrayMapFilter(xs, f) =>
      List(xs, f, xs.elem.eItem, f.elem.eRange.eSnd)
    case ArrayFlatMap(xs, f) =>
      List(xs, f, xs.elem.eItem, f.elem.eRange.eItem)
    case x@ArrayApply(xs, i) =>
      List(xs, i, x.selfType)
    case ArrayApplyMany(xs, is) =>
      List(xs, is, xs.elem.eItem)
    case ArrayLength(xs) =>
      List(xs, xs.elem.eItem)
    case ArrayFind(xs, f) =>
      List(xs, f, xs.elem.eItem)
    case ArraySort(xs, _) =>
      List(xs, xs.elem.eItem)
    case ArraySortBy(xs, f, _) =>
      List(xs, f, f.elem.eDom, f.elem.eRange)
    case ArrayAvg(xs, n) =>
      List(xs, n, xs.elem.eItem)
    case ArrayFold(xs, init, f) =>
      List(xs, init, f, xs.elem.eItem, init.elem)
    case as@ArraySumBy(xs, f, n) =>
      List(xs, f, f.elem.eDom, f.elem.eRange, n)
    case ListLength(xs) =>
      List(xs, xs.elem.eItem)
    case ListMap(xs, f) =>
      List(xs, f, xs.elem.eItem, f.elem.eRange)
    case ListFlatMap(xs, f) =>
      List(xs, f, xs.elem.eItem, f.elem.eRange.eItem)
    case map@MakeMap(_) =>
      val mapElem = map.selfType.asInstanceOf[MMapElem[_, _]]
      List(mapElem.eKey, mapElem.eValue)
    case ab@MakeArrayBuffer(_) =>
      List(ab.selfType.eItem)
    case SemicolonMulti(as, b) =>
      List(as, b, b.elem)
    case _ =>
      super.extractParams(d, paramMirrors)
  }

  override protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case _: CompanionDef[_] =>  //TODO backend
      // ignore companion objects
      m

    case lam: Lambda[a, b] =>
      val mA = elemToManifest(lam.eA).asInstanceOf[Manifest[a]]
      val mB = elemToManifest(lam.eB).asInstanceOf[Manifest[b]]
      val f = m.mirrorLambda[a, b](lam)
      val fun = lms.fun(f)(mA, mB)

      m.addFuncAndSym(sym, f, fun)

    case IsLeft(s) =>
      val sumElem = s.elem.asInstanceOf[SumElem[_, _]]
      (elemToManifest(sumElem.eLeft), elemToManifest(sumElem.eRight)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          val sum = m.symMirror[Either[a, b]](s)
          val exp = lms.either_isLeft(sum)(mA, mB, implicitly[SourceContext])
          m.addSym(sym, exp)
      }

    case IsRight(s) =>
      val sumElem = s.elem.asInstanceOf[SumElem[_, _]]
      (elemToManifest(sumElem.eLeft), elemToManifest(sumElem.eRight)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>
          val sum = m.symMirror[Either[a, b]](s)
          val exp = lms.either_isRight(sum)(mA, mB, implicitly[SourceContext])
          m.addSym(sym, exp)
      }

    case ApplyUnOp(op, arg) =>
      val exp = transformUnOp(m, op, arg)
      m.addSym(sym, exp)

    case ApplyBinOp(op, arg1, arg2) =>
      val exp = transformBinOp(m, op, arg1, arg2)
      m.addSym(sym, exp)

    case i@IfThenElse(cond, ifTrue, ifFalse) =>
      elemToManifest(i.selfType) match {
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

    case ab@ArrayBufferRep(buf) =>
      ab.selfType match {
        case elem: ArrayBufferElem[t] =>
          val exp = m.symMirror[scala.collection.mutable.ArrayBuilder[t]](buf)
          m.addSym(sym, exp)
      }

    case pm@VarMM(map) =>
      pm.selfType match {
        case elem: MMapElem[k, v] =>
          val exp = m.symMirror[MMap[k, v]](map)
          m.addSym(sym, exp)
      }

    case mr@ArrayMapReduce(source, map, reduce) =>
      (source.elem, mr.selfType) match {
        case (ae: ArrayElem[a], me: MMapElem[k, v]) =>
          val mA = elemToManifest(ae.eItem).asInstanceOf[Manifest[a]]
          val mK = elemToManifest(me.eKey).asInstanceOf[Manifest[k]]
          val mV = elemToManifest(me.eValue).asInstanceOf[Manifest[v]]
          val map_ = m.funcMirror[a, (k, v)](map)
          val reduce_ = m.funcMirror[(v, v), v](reduce)

          val exp = source match {
            case Def(range: ArrayRangeFrom0) =>
              val n_ = m.symMirror[Int](range.n)
              lms.rangeMapReduce(n_, map_, reduce_)(mK, mV)
            case Def(ArrayFilter(Def(ArrayMap(Def(range: ArrayRangeFrom0), map1)), filter)) =>
              map1.elem.eRange match {
                case ma: Elem[b] =>
                  val mA = elemToManifest(ma).asInstanceOf[Manifest[b]]
                  val n_ = m.symMirror[Int](range.n)
                  val map1_ = m.funcMirror[Int, b](map1)
                  val filter_ = m.funcMirror[b, Boolean](filter)
                  lms.rangeFilterMapReduce(n_, map1_, filter_, map_, reduce_)(mA, mK, mV)
              }
            case Def(ArrayFilter(source, filter)) =>
              val source_ = m.symMirror[Array[a]](source)
              val filter_ = m.funcMirror[a, Boolean](filter)
              lms.filterMapReduce(source_, filter_, map_, reduce_)(mA, mK, mV)
            case _ =>
              val source_ = m.symMirror[Array[a]](source)
              lms.arrayMapReduce(source_, map_, reduce_)(mA, mK, mV)
          }

          m.addSym(sym, exp)
      }

    case ArrayReduce(source, monoid) =>
      elemToManifest(source.elem.eItem) match {
        case (mA: Manifest[a]) =>
          val src = m.symMirror[Array[a]](source)
          monoid.opName match {
            case "+" =>
              val exp = lms.sumArray[a](src)(mA)
              m.addSym(sym, exp)
            case _ =>
              val (m1, zero, op) = m.mirrorMonoid(monoid.asInstanceOf[RepMonoid[a]])
              val exp = lms.reduceArray(src, zero, op)(mA)
              m1.addSym(sym, exp)
          }
      }

    case ArrayScan(source, monoid) =>
      elemToManifest(source.elem.eItem) match {
        case (mA: Manifest[a]) =>
          val src = m.symMirror[Array[a]](source)
          monoid.opName match {
            //case "+" =>
            //  val exp = lms.sum[a](src)(mA)
            //  m.addSym(sym, exp)
            case _ =>
              val (m1, zero, op) = m.mirrorMonoid(monoid.asInstanceOf[RepMonoid[a]])
              val exp = lms.scanArray(src, zero, op)(mA)
              m1.addSym(sym, exp)
          }
      }

    case ListReduce(source, monoid) =>
      elemToManifest(monoid.eA) match {
        case (mA: Manifest[a]) =>
          val src = m.symMirror[List[a]](source)
          // may want to special-case e.g. sum and product if sumList can be implemented generically
          // (see comment there and implementation for ArrayReduce above)
          val (m1, zero, op) = m.mirrorMonoid(monoid.asInstanceOf[RepMonoid[a]])
          val exp = lms.list_reduce(src, zero, op)(mA)
          m1.addSym(sym, exp)
      }

    // TODO can we make generic version for Reflect?
    case Reflect(array: ArrayBufferEmpty[a], _, _) =>
      implicit val mA = elemToManifest(array.elem.eItem)
      val exp = lms.arraybuilder_make()
      m.addSym(sym, exp)

    case Reflect(PrintlnE(s), _, _) =>
      val s1 = m.symMirror[String](s)
      val exp = lms.println(s1)
      m.addSym(sym, exp)

    case Reflect(ReadLineE(), _, _) =>
      val exp = lms.readline
      m.addSym(sym, exp)

    case Reify(x, u, es) => m
    //    case Reify(x, u, es) =>
    //      createManifest(x.elem) match {
    //        case (mA: Manifest[a]) =>
    //          val x1 = m.symMirror[a](x)
    //          val exp = lms.reify(x1, m.summaryMirror(u), es.map(e => m.symMirror(e)))(mA)
    //          m.addSym(sym, exp)
    //      }

    case _ => super.transformDef(m, g, sym, d)
  }

  protected def transformUnOp[A, B](m: LmsMirror, op: UnOp[A, B], arg: Exp[A]): lms.Exp[_] = {
    implicit val mA = elemToManifest(arg.elem).asInstanceOf[Manifest[A]]
    val _arg = m.symMirror(arg)
    op match {
      case Not => lms.boolean_negate(_arg)
      case NumericNegate(n) =>
        implicit val n1 = n.asInstanceOf[Numeric[A]]
        lms.num_negate(_arg)
      case NumericToDouble(n) => mA match {
        case Manifest.Int => lms.int_to_double(_arg)
        case Manifest.Float => lms.float_to_double(_arg)
        case Manifest.Double => _arg
      }
      case NumericToFloat(n) => mA match {
        case Manifest.Int => lms.int_to_float(_arg)
        case Manifest.Double => lms.double_to_float(_arg)
        case Manifest.Float => _arg
      }
      case NumericToInt(n) => mA match {
        case Manifest.Float => lms.float_to_int(_arg)
        case Manifest.Double => lms.double_to_int(_arg)
        case Manifest.Long => lms.long_to_int(_arg)
        case Manifest.Int => _arg
      }
      case NumericToLong(n) => mA match {
        case Manifest.Int => lms.int_to_long(_arg)
        case Manifest.Long => _arg
      }
      case NumericToString() => lms.String.valueOf(_arg)
      case HashCode() => lms.hash_code(_arg)
      case StringToInt => lms.string_toint(_arg)
      case BooleanToInt => lms.boolean_to_int(_arg)
      case StringToDouble => lms.string_todouble(_arg)
      case MathExp => lms.math_exp(_arg)
      case MathSin => lms.math_sin(_arg)
      case MathSqrt => lms.math_sqrt(_arg)
      case MathLog => lms.math_log(_arg)
      case MathAbs(n) =>
        implicit val n1 = n.asInstanceOf[Numeric[A]]
        lms.math_abs(_arg)
    }
  }

  def transformBinOp[A, B](m: LmsMirror, op: BinOp[A, B], arg1: Exp[A], arg2: Exp[A]): lms.Exp[_] = {
    implicit val mA = elemToManifest(arg1.elem).asInstanceOf[Manifest[A]]
    val _arg1 = m.symMirror(arg1)
    val _arg2 = m.symMirror(arg2)

    op match {
      case NumericTimes(n) =>
        implicit val n1 = n.asInstanceOf[Numeric[A]]
        lms.numeric_times(_arg1, _arg2)
      case NumericPlus(n) =>
        implicit val n1 = n.asInstanceOf[Numeric[A]]
        lms.numeric_plus(_arg1, _arg2)
      case NumericMinus(n) =>
        implicit val n1 = n.asInstanceOf[Numeric[A]]
        lms.numeric_minus(_arg1, _arg2)
      case IntegralDivide(n) =>
        implicit val n1 = n.asInstanceOf[Numeric[A]]
        lms.numeric_divide(_arg1, _arg2)
      case IntegralMod(n) =>
        implicit val n1 = n.asInstanceOf[Integral[A]]
        lms.integral_mod(_arg1, _arg2)
      case FractionalDivide(n) =>
        implicit val n1 = n.asInstanceOf[Numeric[A]]
        lms.numeric_divide(_arg1, _arg2)
      case Equals() =>
        lms.equals(_arg1, _arg2)
      case OrderingLT(ord) =>
        implicit val ord1 = ord.asInstanceOf[Ordering[A]]
        lms.ordering_lt(_arg1, _arg2)
      case OrderingLTEQ(ord) =>
        implicit val ord1 = ord.asInstanceOf[Ordering[A]]
        lms.ordering_lteq(_arg1, _arg2)
      case OrderingGT(ord) =>
        implicit val ord1 = ord.asInstanceOf[Ordering[A]]
        lms.ordering_gt(_arg1, _arg2)
      case OrderingGTEQ(ord) =>
        implicit val ord1 = ord.asInstanceOf[Ordering[A]]
        lms.ordering_gteq(_arg1, _arg2)
      case OrderingMax(ord) =>
        implicit val ord1 = ord.asInstanceOf[Ordering[A]]
        lms.ordering_max(_arg1, _arg2)
      case OrderingMin(ord) =>
        implicit val ord1 = ord.asInstanceOf[Ordering[A]]
        lms.ordering_min(_arg1, _arg2)
      case OrderingCompare(ord) =>
        implicit val ord1 = ord.asInstanceOf[Ordering[A]]
        lms.ordering_compare(_arg1, _arg2)
      case And =>
        lms.boolean_and(_arg1, _arg2)
      case Or =>
        lms.boolean_or(_arg1, _arg2)
      case StringConcat =>
        lms.string_plus(_arg1, _arg2)
      case StringContains =>
        lms.string_contains(_arg1, _arg2)
      case StringStartsWith =>
        lms.string_startswith(_arg1, _arg2)
      case StringEndsWith =>
        lms.string_endsWith(_arg1, _arg2)
      case StringMatches =>
        lms.string_matches(_arg1, _arg2)
      case MathPow =>
        lms.math_pow(_arg1, _arg2)
    }
  }
}
