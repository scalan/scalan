package scalan.dynamic

import java.lang.reflect.Method

import scalan.{Lazy, _}
import scalan.universe.api.TypesApi
import scalan.universe.api.UniverseUtils._
import scalan.util.CollectionUtil._

trait Specializations extends Views with Converters { self: Scalan =>

  type RepIsoFunc[T,R,M] = Rep[IsoFunc[T,R,M]]
  trait IsoFunc[T,R,M] extends Def[IsoFunc[T,R,M]] {
    implicit def eT: Elem[T]
    implicit def eR: Elem[R]
    implicit def eM: Elem[M]
    def func: Rep[T => R]
    def metric: Rep[T => M]
    def apply(x: Rep[T]): Rep[R]
  }

  abstract class IsoFuncBase[T,R,M]
        (val func: Rep[T => R], val metric: Rep[T => M])
//        (implicit val eT: Elem[T], val eR: Elem[R], val eM: Elem[M])
    extends IsoFunc[T,R,M] {
    implicit def eT: Elem[T]
    implicit def eR: Elem[R]
    implicit def eM: Elem[M]
    def apply(x: Rep[T]): Rep[R] = func(x)
    override def toString: String = s"${eT.name} iso≈> ${eR.name}"
    override def equals(other: Any): Boolean = other match {
      case c: Specializations#IsoFuncBase[_, _, _] =>
        eT == c.eT && eR == c.eR && eM == c.eM && func == c.func && metric == c.metric
      case _ => false
    }
  }
}

trait SpecializationsModule extends impl.SpecializationsDefs with TypesApi { scalan: Scalan =>

  trait AbstractSpecKey[F] {
    def format: F
  }

  case class NoFuse[A:Elem](arg: Rep[A]) extends Def[A] {
    def selfType = element[A]
//    override def mirror(t: Transformer) = NoFuse(t(arg))
  }
  def no_fuse[A:Elem](arg: Rep[A]) = NoFuse(arg)

  def composeKernel[S:Elem,T:Elem,A:Elem,B:Elem,C](isoA: Iso[S,A], f: Rep[A=>B], isoC: Iso[T,C]): List[Rep[S=>T]] = {
    (element[B], isoC.eTo) match {
      case (eB,eC) if (eB==eC) =>
        val k = fun { x: Rep[S] =>
          val a = isoA.to(x)
          val b = f(a).asRep[C]
          isoC.from(b)
        }
        List(k)
      case HasConv(conv) =>
        try {
          val k = fun { x: Rep[S] =>
            val a = isoA.to(x)
            val b = f(a)
            val converted = conv(b)
            isoC.from(converted)
          }
          List(k)
        }
        catch {
          case _: Throwable => Nil
        }
      case _ => Nil
    }
  }

//  def composeWrapper[S,T,A,B,C](isoA: Iso[S,A], f: Rep[A=>B], isoC: Iso[T,C]): List[Rep[S=>T]] = {
//    (typeOf[B], isoC.tTo) match {
//      case HasConv(conv) =>
//        List(fun { x: Exp[S] =>
//          val a = isoA.to(x)
//          val b = f(a)
//          val converted = conv(b)
//          isoC.from(converted)
//        })
//      case _ => Nil
//    }
//  }

  def getReprIsos[A](eA: Elem[A])(implicit params: QueryParams): List[Iso[_,A]] = {
    val eA_subty = eA.allSpecs(params).diff(List(eA))
    val eA_concrete_subty = eA_subty.filter(_.isConcrete)
    val isos = eA_concrete_subty.map {
      case e: Elem[A]@unchecked => getIsoByElem(e)
      case e => !!!(s"Invalid subtype $e for $eA")
    }
    isos
  }

  def directSpecs[A,B](iso: Iso[A,B])(implicit params: QueryParams): List[Iso[_,B]] = {
    val eA = iso.eFrom
    val isos = getReprIsos(eA)
    val res = isos.map({
      case i: Iso[a,A] @unchecked =>
        val eA: Elem[A] = i.eTo
        val eB: Elem[B] = iso.eTo
        tryComposeIso(iso, i)
    }).collect { case Some(i) => i }
    res.distinct
  }

//  def directSpecs2[A,B](iso: Iso[A,B]): List[Iso[_,B]] = {
//    val tA = iso.tFrom
//    val tB = iso.tTo
//    val allspecs = allSpecs(tA)
//    val tSpecs = allspecs.diff(List(tA,tB)).filter(_.isConcrete)
//    val isos = tSpecs.map(t => getIsoByType(t))
//    val res = isos.map(i => composeIso(iso, i))
//    res.distinct
//  }

  def allSpecs[A,B](iso: Iso[A,B])(implicit params: QueryParams): List[Iso[_,B]] =
    iso.traverseDepthFirst(directSpecs(_).map(_.asInstanceOf[Iso[A,B]])).distinct

  def getTypeSpecs(t: TypeDesc)(implicit params: QueryParams): List[Elem[_]] = {
    val inSpecs = t.allConcreteSpecs
    inSpecs.map(_.asElem)
  }

  def getAllReprIsos[A](eA: Elem[A])(implicit params: QueryParams): List[Iso[_,A]] = {
    val p = params.typeFilter
    val iso = getIsoByElem(eA)
    val specs = allSpecs(iso)
    specs.filter(iso => iso.eFrom.isConcrete && p(iso.eFrom))
  }

  def getIsoSpecs(typeSpecs: List[Elem[_]])(implicit params: QueryParams): List[Iso[Any,Any]] = {
    for {
      e <- typeSpecs
      iso <- getAllReprIsos(e).asInstanceOf[List[Iso[Any,Any]]]
    }
    yield iso
  }

  def getFormats(isos: List[Iso[Any,Any]]) = isos.map(_.eFrom).distinct

  def composeKernel[A,B:Elem](inE: Elem[_], outE: Elem[_], f: Rep[A=>B], whereIn: TypePredicate, whereOut: TypePredicate): List[Rep[_]] = {
    (inE, outE) match {
      case (inE: Elem[a], outE: Elem[b]) =>
        val ins = getAllReprIsos(inE)(QueryParams(true, whereIn))
        val outs = getAllReprIsos(outE)(QueryParams(true, whereOut))
        val kernels = for {
          in  <- ins
          out <- outs
        }
        yield  (in, out) match {
          case (inIso: Iso[s,a], outIso: Iso[t,b]) =>
            implicit val es = inIso.eFrom
            implicit val ea = inIso.eTo
            implicit val et = outIso.eFrom
            implicit val eb = outIso.eTo
            for {
              kernel <- scalan.composeKernel(inIso, f.asRep[a=>B], outIso)
            }
            yield kernel
        }
        val res: List[Rep[_]] = kernels.flatten
        res
    }
  }

//  def genWrappers[A,B](inT: Type, outT: Type, f: Rep[A=>B], inType: TypePredicate, outType: TypePredicate): List[Rep[_]] = {
//    val ins = allSpecs(getIsoByType(inT)).filter(iso => iso.tFrom.isConcrete && inType(iso.tFrom))
//    val outs = allSpecs(getIsoByType(outT)).filter(iso => iso.tFrom.isConcrete && outType(iso.tFrom))
//    val wrappers = for {
//      in  <- ins
//      out <- outs
//      w <- composeWrapper(in, f, out)
//    }
//    yield w
//    wrappers.flatten
//  }

  def specialize[A,B](f: Rep[A=>B], inFilter: TypePredicate = AllTypes, outFilter: TypePredicate = AllTypes)(implicit eA: Elem[A], eB:Elem[B]): List[Sym] = {
    val inElems = eA.allConcreteSpecs(QueryParams(true))
    val outElems = eB.allConcreteSpecs(QueryParams(true))
    val specs = for {
      inE <- inElems
      outE <- outElems
      k <- composeKernel(inE.asElem, outE.asElem, f, inFilter, outFilter)
    } yield k
    specs.distinct
  }

  type ConverterMatrix = Map[(Elem[_],Elem[_]), Set[Conv[Any,Any]]]

  // key for adressing specializations contains 4 elements
  // input,output - elems for kernel in/out
  // convertedTo - elem which input was converted to
  // (optionally) intermediateFormat  - before specialization, input can be converted to this format
  case class SpecKey(eIn: Elem[_], eOut: Elem[_], convTo: Elem[_], intermediateFormat: Option[Elem[_]]) extends AbstractSpecKey[Elem[_]] {
    def format = eIn
  }


  def buildConvMatrix(esFrom: List[Elem[_]], esTo: List[Elem[_]], convSet: (Elem[Any],Elem[Any]) => Set[Conv[Any,Any]]): ConverterMatrix = {
    esFrom.flatMap { case ea: Elem[Any] @unchecked =>
    esTo.map { case eb: Elem[Any]@unchecked =>
      ((ea, eb), convSet(ea,eb))
    }}.toMap
  }

  def buildFormatConvMatrix(
        formatsFrom: List[Elem[_]],
        formatsTo: List[Elem[_]],
        isosFrom: List[Iso[Any,Any]],
        isosTo: List[Iso[Any,Any]]): ConverterMatrix = {
    def buildConvSet(ea: Elem[Any], eb: Elem[Any]): Set[Conv[Any,Any]] = {
      val converters = for {
        iso1 <- isosFrom.filter(_.eFrom == ea)
        iso2 <- isosTo.filter(_.eFrom == eb)
        c    <- HasConv.unapply((iso1.eTo, iso2.eTo))
      }
      yield {
        BaseConverter(fun({ x: Rep[Any] =>
            val a1 = iso1.to(x)
            val a2 = c(a1)
            iso2.from(a2)
          })(Lazy(iso1.eFrom)))
      }
      converters.toSet
    }
    buildConvMatrix(formatsFrom, formatsTo, buildConvSet)
  }

  def applySpecificKernel[A,B,A1,B1](kernel: DynKernel[A,B], arg: Rep[A1], specNum: Rep[Int], eOut: Elem[B1]) = {
    implicit val ea1 = arg.elem
    implicit val eb = kernel.eB
    val iso = identityIso[A1]
    ea1.getDataIso match {
      case iso: Iso[c,A1] @unchecked =>
        // TODO: assert(filtered list is empty)
        val isoOut = kernel.outFormatIsos.filter({i => i.eFrom equals eOut}).head.asInstanceOf[Iso[B1,B]]
        ApplyKernel(kernel.self, iso.from(arg), iso, isoOut, specNum)
    }
  }

  def updateDynKernel[A,B](dynKernel: DynKernel[A,B], inTypes: TypePredicate, outTypes: TypePredicate) = {
    DynKernel[A,B](dynKernel, inTypes, outTypes)
  }

  case class DynKernel[A,B](
        func: Rep[A => B],    // original function
        inTypes: TypePredicate, outTypes: TypePredicate)
        (implicit val eA: Elem[A] = func.elem.eDom, val eB: Elem[B] = func.elem.eRange)
        extends BaseDef[A=>B]
  {
    val isConcreteOutType = eB.isConcrete
    def isDefaultOutType(el: Elem[_]): Boolean = isConcreteOutType && (el == eB)

    val inTypesCurr     : TypePredicate = inTypes
    val outTypesCurr     : TypePredicate = outTypes
    val originalFunc: Rep[A=>B] = func

    /*lazy*/ val inTypeSpecs : List[Elem[_]] = getTypeSpecs(eA)(QueryParams(true, inTypes))
    /*lazy*/ val outTypeSpecs: List[Elem[_]] = getTypeSpecs(eB)(QueryParams(true, outTypes))

    /*lazy*/ val inIsoSpecs : List[Iso[Any,Any]] = getIsoSpecs(inTypeSpecs)(QueryParams(true, inTypes))
    /*lazy*/ val outIsoSpecs: List[Iso[Any,Any]] = getIsoSpecs(outTypeSpecs)(QueryParams(true, outTypes))

    /*lazy*/ val inFormatIsos:  List[Iso[Any,Any]] = List[Iso[Any,Any]]()

    /*lazy*/ val outFormatIsosIncrement = outIsoSpecs.filter (iso => outTypesCurr(iso.eFrom))
    /*lazy*/ val outFormatIsosDefault = outIsoSpecs.filter( iso => isDefaultOutType(iso.eFrom))
    /*lazy*/ val outFormatIsosParent:  List[Iso[Any,Any]] = outFormatIsosDefault
    /*lazy*/ val outFormatIsos: List[Iso[Any,Any]] = (outFormatIsosDefault ++ outFormatIsosParent ++ outFormatIsosIncrement).distinct

    /*lazy*/ val inFormatsIncrement: List[Elem[Any]] = getFormats(inFormatIsos)
    /*lazy*/ val inFormatsParent: List[Elem[Any]] = List[Elem[Any]]()
    /*lazy*/ val inFormats : List[Elem[Any]] = inFormatsParent ++ inFormatsIncrement

    /*lazy*/ val outFormatsIncrement: List[Elem[Any]] = getFormats(outFormatIsosIncrement)
    /*lazy*/ val outFormatsParent: List[Elem[Any]] = List[Elem[Any]]()
    /*lazy*/ val outFormats: List[Elem[Any]] = outFormatsParent ++ outFormatsIncrement

    /*lazy*/ val inConvMatrix: ConverterMatrix  = buildConvMatrix(inTypeSpecs, inTypeSpecs, (ea,eb)=>HasConv.unapply((ea, eb)).toSet)
//    lazy val outConvMatrix = buildConvMatrix(outTypeSpecs)

      val inFormatConvMatrix = buildFormatConvMatrix(inFormats, inFormats, inFormatIsos, outFormatIsos)
//    /*lazy*/ val inFormatConvMatrixIncrement = buildFormatConvMatrix(inFormatsParent, inFormatsIncrement, inFormatIsos, inFormatIsosIncrement) ++
//                                           buildFormatConvMatrix(inFormatsIncrement, inFormatsParent, inFormatIsosIncrement, inFormatIsosParent) ++
//                                           buildFormatConvMatrix(inFormatsIncrement, inFormatsIncrement, inFormatIsosIncrement, inFormatIsosIncrement)
//    /*lazy*/ val inFormatConvMatrixParent = parent.map(_.inFormatConvMatrix).getOrElse( Map[(Elem[_],Elem[_]), Set[Conv[Any,Any]]]() )

    def getPrimaryKernels(inIsos: List[Iso[Any,Any]], outIsos: List[Iso[Any,Any]]): List[Rep[Any=>Any]] = {
      getPrimaryKernelsMap(inIsos, outIsos).values.toList.distinct
    }

    def getInputConvertersForElem[A1,A2](matrix: ConverterMatrix, eFrom: Elem[A1]): List[Conv[A1,A2]] = {
      val res = for {
        e1e2 <- matrix.keys.filter { case (e1, e2) => e1 == eFrom }
        cs   <- matrix.get(e1e2).toIterable
        c    <- cs
      } yield c.asRep[Converter[A1,A2]]
      res.toList
    }
    def getInputConvertersToElem[A1,A2](matrix: ConverterMatrix, eTo: Elem[A2]): List[Conv[A1,A2]] = {
      val res = for {
        e1e2 <- matrix.keys.filter { case (e1, e2) => e2 == eTo }
        cs   <- matrix.get(e1e2).toIterable
        c    <- cs
      } yield c.asRep[Converter[A1,A2]]
      res.toList
    }

    // Just getting kernels as List[Rep[Any=>Any]] is not enough. We need the way to address them somehow.
    // So, we return Map, which contains each kernel addressed by SpecKey
    def getPrimaryKernelsMap(inIsos: List[Iso[Any,Any]], outIsos: List[Iso[Any,Any]]): Map[SpecKey, Rep[Any =>Any]] = {
      val kernels = for {
        inIso <- inIsos
        outIso <- outIsos
      }
      yield  (inIso, outIso) match {
          case (inIso: Iso[s,a], outIso: Iso[t,b]) =>
            implicit val es: Elem[s] = inIso.eFrom
            implicit val ea: Elem[a] = inIso.eTo
            implicit val et: Elem[t] = outIso.eFrom
            implicit val eb: Elem[b] = outIso.eTo
            val cs = getInputConvertersForElem(inConvMatrix, ea)
            val res = for {
              csym @ Def(c: Converter[_,_]) <- cs
              kernel <- {
                val f = fun({ x: Rep[a] =>
                  val a1 = csym.asRep[Converter[a,A]].apply(x)
                  val b = originalFunc(a1)
                  b
                })(Lazy(ea))
                composeKernel(inIso, f.asRep[a=>B], outIso)(es, et, ea, eB)
              }
            }
            yield {
              (SpecKey(es, et, c.eR, None), kernel.asRep[Any => Any])
            }
            res
        }
      kernels.flatten.toMap
    }

//    /*lazy*/ val primaryKernelsMapIncrement = {
//      val r1 = getPrimaryKernelsMap(inFormatIsosParent, outFormatIsosIncrement) //++
//      val r2 = getPrimaryKernelsMap(inFormatIsosIncrement, outFormatIsosParent) //++
//      val r3 = getPrimaryKernelsMap(inFormatIsosIncrement, outFormatIsosIncrement)
//      r1 ++ r2 ++ r3
//    }
    /*lazy*/ val primaryKernelsMap = Map[SpecKey, Rep[Any=>Any]]()

    /*lazy*/ val primaryKernels= primaryKernelsMap.values.toList.distinct

    val extendedKernelsMapIncrement: Map[SpecKey, Rep[Any =>Any]] = {
      // Converters from new formats to all others
      val kernelsFrom = for {
        inF <- inFormatsIncrement
        csym @ Def(c: Converter[_,_]) <- getInputConvertersForElem[Any,Any](inFormatConvMatrix, inF)
        (sKey,primK) <- primaryKernelsMap.filter {case (key, f @ Def(l: Lambda[_,_])) => l.eA == c.eR }
      }
      yield {
        (inF equals c.eR) match {
          case false => {
            val kernel = fun({ x: Rep[Any] => primK(no_fuse(csym(x))(c.eR.asElem[Any])) })(Lazy(c.eT.asElem[Any]))
            val new_key = SpecKey(c.eT, sKey.eOut, sKey.convTo, Some(c.eR))
            (new_key, kernel)
          }
          case _ => (sKey, primK)
        }
      }
      // Converters from all other formats to new formats
      val kernelsTo = for {
        inF <- inFormatsIncrement
        csym @ Def(c: Converter[_,_]) <- getInputConvertersToElem[Any,Any](inFormatConvMatrix, inF)
        (sKey,primK) <- primaryKernelsMap.filter {case (key, f @ Def(l: Lambda[_,_])) => l.eA == c.eR }
      }
      yield {
        (c.eT equals c.eR) match {
          case false => {
            val kernel = fun({ x: Rep[Any] => primK(no_fuse(csym(x))(c.eR.asElem[Any])) })(Lazy(c.eT.asElem[Any]))
            val new_key = SpecKey(c.eT, sKey.eOut, sKey.convTo, Some(c.eR))
            (new_key, kernel)
          }
          case _ => (sKey, primK)
        }
      }

      // Converters from all formats using new primary kernels
      val kernelsNewPrimary = for {
        inF <- inFormats
        csym @ Def(c: Converter[_,_]) <- getInputConvertersForElem[Any,Any](inFormatConvMatrix, inF)
        (sKey,primK) <- primaryKernelsMap.filter {case (key, f @ Def(l: Lambda[_,_])) => l.eA == c.eR }
      }
      yield {
        (c.eT equals c.eR) match {
          case false => {
            val kernel = fun({ x: Rep[Any] => primK(no_fuse(csym(x))(c.eR.asElem[Any])) })(Lazy(c.eT.asElem[Any]))
            val new_key = SpecKey(c.eT, sKey.eOut, sKey.convTo, Some(c.eR))
            (new_key, kernel)
          }
          case _ => (sKey, primK)
        }
      }
      (kernelsNewPrimary ++ kernelsFrom ++ kernelsTo).distinct.toMap
    }
    /*lazy*/ val extendedKernelsMap:  Map[SpecKey, Rep[Any =>Any]]  = Map[SpecKey, Rep[Any=>Any]]()
    /*lazy*/ val extendedKernels = extendedKernelsMap.values.toList.distinct.diff(primaryKernels)
  }

//  case class DynKernelSpecs[A,B](nKernels: Rep[Int], specList: List[Exp[A=>B]])(implicit val eA:Elem[A], val eB: Elem[B]) extends ArrayDef[A=>B] {
//    lazy val eT = element[A=>B]
////    override def mirror(t: Transformer) = DynKernelSpecs(t(nKernels), specList.map{s => t(s)})
//  }

  case class ApplyKernel[A, B, C, A1, D, B1]
        (kernel: Exp[A => B], arg: Exp[C], isoDom: Iso[C,A1], isoRange: Iso[D,B1], specNum: Rep[Int])
    extends Def[D]
  {
    // TODO assert(A1 isSpecialOf A && B1 isSpecialOf B)
    def selfType = isoRange.eFrom
//    override def mirror(t: Transformer) = ApplyKernel(t(kernel), t(arg), isoDom, isoRange, t(specNum))
  }

  def isoFun[A:Elem, B: Elem](
        f: Rep[A] => Rep[B],
        inTypes: TypePredicate = AllTypes, outTypes: TypePredicate = AllTypes) = {
    DynKernel(fun(f), inTypes, outTypes)
  }

  def exactTypes(elems: Elem[_]*): TypePredicate = {
    el => elems.contains(el)
  }

//  def specializeKernel[A,B,C,D](k: Rep[A => B])(implicit eC: Elem[C], eD:Elem[D]): Rep[C => D] = {
//    k match {
//      case Def(k: IsoKernel[A,B,m]) =>
//        implicit val eA = k.eA
//        implicit val eB = k.eB
//        implicit val em = k.eM
//        implicit val nm = k.nM
//        val k1 = isoFun[A,B,m](x => k.func(x), x => k.metric(x), exactType[C], exactType[D])
//
//      case f @ Def(l: Lambda[A,B] @unchecked) =>
//        implicit val eA = l.eA
//        implicit val eB = l.eB
//        isoFun[A,B,Int](x => f(x), _ => 0, exactType[C], exactType[D])
//      case Def(d) => !!!(s"Don't know how to specialize kernel $d")
//      case _ => !!!(s"Don't know how to specialize kernel $k")
//    }
//  }

//  def getSpecialization[R](specElem: Elem[_], method: Method, args: List[AnyRef])(implicit eR: Elem[R]): OneMethodCallSpec = {
//    val iso = specElem.getDataIso
//    iso match {
//      case iso: Iso[data, o1] =>
//        val eD = iso.eFrom
//        MethodCallSpec(iso, fun[data,R]({ d: Rep[data] =>
//          mkMethodCall(iso.to(d), method, args, false).asRep[R]
//        })(Lazy(eD), eR))
//    }
//  }
//
//  def getSpecializations[R](receiverElem: Elem[_], method: Method, args: List[AnyRef])(implicit eR: Elem[R]): List[OneMethodCallSpec] = {
//    val eObj = receiverElem
//    val tObj = Type(eObj)
//    val specs = tObj.directSpecsExclusive
//    val specElems = specs.asElems
//    val res = specElems.map(getSpecialization(_, method, args))
//    res
//  }

  override def rewriteDef[T](d: Def[T]) = d match {

    case Apply(Def(k: DynKernel[a,b]), x: Rep[a1]) =>
      implicit val ea1 = x.elem
      implicit val eb = k.eB
      ea1.getDataIso match {
        case iso: Iso[c,a1] @unchecked =>
          // If DynKernel is called directly - we suppose it's specialization number 0 is going to be called
          ApplyKernel(k.self, iso.from(x), iso, identityIso[b], 0)
      }

    case ApplyKernel(
          Def(k: DynKernel[a,b]),
          HasViews(_x, iso: Iso[_,_]), isoDom: Iso[c,a1], isoRange: Iso[d,b1], specNum) =>
      implicit val eb = k.eB
      iso match {
        case iso: Iso[s, c] @unchecked =>
          val x = _x.asRep[s]
          ApplyKernel(k.self, x, composeIso(isoDom, iso), isoRange, specNum)
      }
    case NoFuse(Def(k@Tup(f,s))) => {
      k.selfType match {
        case e: PairElem[_, _] => Pair(no_fuse(f)(e.eFst), no_fuse(s)(e.eSnd))
      }
    }

//    case mc @ MethodCall(appSym @ Def(appDef: ApplyKernel[a,b,c,a1,d,b1]), m, args, _)
//          if !mc.neverInvoke && mc.canBeSpecialized =>
//      val specs = getSpecializations(appSym.elem, m, args)(mc.selfType)
//      val newCall = mkMethodCallWithSpecs(appSym, m, args)(specs)
//      newCall

//    case mc2 @ MethodCall(mcSym1 @ Def(mc1: MethodCall), m, args, _)
//          if !mc1.specializations.isEmpty && mc2.canBeSpecialized =>
//      val objSpecs = mc1.specializations.flatMap { spec =>
//        val eRes = spec.f.elem.eRange
//        val tRes = Type(eRes)
//        val resSpecs = tRes :: tRes.directSpecsExclusive
//        resSpecs.filter(_.isConcrete)
//      }.distinct
//      val specs = objSpecs.asElems.map { spec => getSpecialization(spec, m, args)(mc2.selfType) }
//      val newCall = mkMethodCallWithSpecs(mcSym1, m, args)(specs)
//      newCall

    case _ => super.rewriteDef(d)
  }
}