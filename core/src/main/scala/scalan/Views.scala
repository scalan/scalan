package scalan

import java.lang.reflect.Method

import scala.language.higherKinds
import scala.collection.mutable.{Map => MutMap}

trait Views extends TypeDescs with Proxy { self: ViewsModule with Scalan =>
  type Iso[From, To] = Rep[IsoUR[From, To]]

  // TODO try to find a way to generate eTo such that equals and hashCode can refer to it (see commented code)
  trait IsoUR[From, To] extends Def[IsoUR[From, To]] {
    def eFrom: Elem[From]
    def eTo: Elem[To]
    def from(p: Rep[To]): Rep[From]
    def to(p: Rep[From]): Rep[To]
    override def toString = s"${eFrom.name} <-> ${eTo.name}"
    override def equals(other: Any): Boolean =
      !!!(s"Iso.equals must be overridden in $getClass. Make sure the outer reference is ignored when overriding.")
    override lazy val hashCode = 41 * eFrom.hashCode // + eTo.hashCode
    def isIdentity: Boolean = false
    lazy val fromFun = fun { x: Rep[To] => from(x) }(Lazy(eTo))
    lazy val toFun = fun { x: Rep[From] => to(x) }(Lazy(eFrom))

    //    if (isDebug) {
    //      debug$IsoCounter(this) += 1
    //    }
  }

  abstract class IdentityIso[A](implicit val eA: Elem[A]) extends IsoUR[A, A] {
    def eFrom: Elem[A] = eA
    def eTo: Elem[A] = eA
    def from(x: Rep[A]) = x
    def to(x: Rep[A]) = x
    override def isIdentity = true
    override def equals(other: Any) = other match {
      case i: Views#IdentityIso[_] => (this eq i) || (eFrom == i.eFrom)
      case _ => false
    }
  }
  implicit class IsoOps[A,B](iso: Iso[A,B]) {
    def >>[C](iso2: Iso[B,C]): Iso[A,C] = composeIso(iso2, iso)
  }
  implicit class AnyIsoOps(iso: Iso[_, _]) {
    def asIso[C,D] = iso.asInstanceOf[Iso[C,D]]
  }

  // TODO we can get eA1 etc. from iso1 and iso2, but this won't work as default arguments
  // because this creates a compiler-generated companion object and conflicts with `def PairIsoUR`
  // in ViewsImpl.scala
  abstract class PairIso[A1, A2, B1, B2](val iso1: Iso[A1, B1], val iso2: Iso[A2, B2])
    extends IsoUR[(A1, A2), (B1, B2)] {
    implicit def eA1: Elem[A1]; implicit def eA2: Elem[A2]; implicit def eB1: Elem[B1]; implicit def eB2: Elem[B2]
    lazy val eFrom: Elem[(A1, A2)] = element[(A1, A2)]
    lazy val eTo: Elem[(B1, B2)] = element[(B1, B2)]

    // null is used since the only reason this exists is performance
    // TODO consider removing completely
    var fromCacheKey: Rep[(B1,B2)] = null.asInstanceOf[Rep[(B1,B2)]]
    var fromCacheValue: Rep[(A1,A2)] = null.asInstanceOf[Rep[(A1,A2)]]
    var toCacheKey: Rep[(A1,A2)] = null.asInstanceOf[Rep[(A1,A2)]]
    var toCacheValue: Rep[(B1,B2)] = null.asInstanceOf[Rep[(B1,B2)]]

    def from(b: Rep[(B1, B2)]) = {
      if (cachePairs) {
        // b is not null, so the condition includes fromCacheKey == null
        if (b != fromCacheKey) {
          fromCacheKey = b
          fromCacheValue = Pair(iso1.from(b._1), iso2.from(b._2))
        }
        fromCacheValue
      } else
        Pair(iso1.from(b._1), iso2.from(b._2))
    }

    def to(a: Rep[(A1, A2)]) = {
      if (cachePairs) {
        // a is not null, so the condition includes toCacheKey == null
        if (a != toCacheKey) {
          toCacheKey = a
          toCacheValue = Pair(iso1.to(a._1), iso2.to(a._2))
        }
        toCacheValue
      } else
        Pair(iso1.to(a._1), iso2.to(a._2))
    }
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#PairIso[_, _, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }
  trait PairIsoCompanion

  abstract class AbsorbFirstUnitIso[A2,B2](val iso2: Iso[A2, B2]) extends IsoUR[A2, (Unit, B2)] {
    implicit def eA2: Elem[A2]; implicit def eB2: Elem[B2]
    lazy val eFrom = eA2
    lazy val eTo: Elem[(Unit, B2)] = element[(Unit, B2)]
    def from(b: Rep[(Unit, B2)]) = {
      iso2.from(b._2)
    }
    def to(a: Rep[A2]) = {
      Pair((), iso2.to(a))
    }
    override def isIdentity = false
    override def equals(other: Any) = other match {
      case i: Views#AbsorbFirstUnitIso[_, _] => (this eq i) || (iso2 == i.iso2)
      case _ => false
    }
  }

  abstract class AbsorbSecondUnitIso[A1,B1](val iso1: Iso[A1, B1]) extends IsoUR[A1, (B1,Unit)] {
    implicit def eA1: Elem[A1]; implicit def eB1: Elem[B1]
    lazy val eFrom = eA1
    lazy val eTo: Elem[(B1,Unit)] = element[(B1,Unit)]
    def from(b: Rep[(B1,Unit)]) = {
      iso1.from(b._1)
    }
    def to(a: Rep[A1]) = {
      Pair(iso1.to(a), ())
    }
    override def isIdentity = false
    override def equals(other: Any) = other match {
      case i: Views#AbsorbSecondUnitIso[_, _] => (this eq i) || (iso1 == i.iso1)
      case _ => false
    }
  }

  abstract class SumIso[A1, A2, B1, B2](val iso1: Iso[A1, B1], val iso2: Iso[A2, B2])
    extends IsoUR[A1 | A2, B1 | B2] {
    implicit def eA1: Elem[A1]; implicit def eA2: Elem[A2]; implicit def eB1: Elem[B1]; implicit def eB2: Elem[B2]
    lazy val eFrom: Elem[A1 | A2] = element[A1 | A2]
    lazy val eTo: Elem[B1 | B2] = element[B1 | B2]
    def from(b: Rep[B1 | B2]) =
      b.mapSumBy(iso1.fromFun, iso2.fromFun)
    def to(a: Rep[A1 | A2]) =
      a.mapSumBy(iso1.toFun, iso2.toFun)
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#SumIso[_, _, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }

  abstract class ComposeIso[A, B, C](val iso2: Iso[B, C], val iso1: Iso[A, B])/*(
    implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])*/ extends IsoUR[A, C] {
    def eFrom: Elem[A] = iso1.eFrom
    def eTo: Elem[C] = iso2.eTo
    def from(c: Rep[C]) = iso1.from(iso2.from(c))
    def to(a: Rep[A]) = iso2.to(iso1.to(a))
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#ComposeIso[_, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }

  abstract class FuncIso[A, B, C, D](val iso1: Iso[A, B], val iso2: Iso[C, D])/*(
    implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C], val eD: Elem[D])*/
    extends IsoUR[A => C, B => D] {
    implicit def eA: Elem[A]; implicit def eB: Elem[B]; implicit def eC: Elem[C]; implicit def eD: Elem[D]
    lazy val eFrom: Elem[A => C] = element[A => C]
    lazy val eTo: Elem[B => D] = element[B => D]
    def from(f: Rep[B => D]): Rep[A => C] = {
      fun { b => iso2.from(f(iso1.to(b))) }
    }
    def to(f: Rep[A => C]): Rep[B => D] = {
      fun { a => iso2.to(f(iso1.from(a))) }
    }
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#FuncIso[_, _, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }

  abstract class ConverterIso[A, B](val convTo: Conv[A,B], val convFrom: Conv[B,A])
//                                   (implicit val eA: Elem[A], val eB: Elem[B])
    extends IsoUR[A,B] {
    def eA: Elem[A]; def eB: Elem[B]
    def eFrom: Elem[A] = eA
    def eTo: Elem[B] = eB
    def to(a: Rep[A]) = convTo(a)
    def from(b: Rep[B]) = convFrom(b)
    override lazy val toFun = convTo.convFun
    override lazy val fromFun = convFrom.convFun
    override def isIdentity = false
    override def equals(other: Any) = other match {
      case i: Views#ConverterIso[_, _] => (this eq i) || (convTo == i.convTo && convFrom == i.convFrom)
      case _ => false
    }
  }

  type Iso1[A, B, C[_]] = Rep[Iso1UR[A, B, C]]

  trait Iso1UR[A, B, C[_]]
    extends IsoUR[C[A], C[B]] {
    def innerIso: Iso[A, B]
    implicit def cC: Cont[C]
    implicit def eA: Elem[A] = innerIso.eFrom
    implicit def eB: Elem[B] = innerIso.eTo
    lazy val eFrom: Elem[C[A]] = cC.lift(innerIso.eFrom)
    lazy val eTo: Elem[C[B]] = cC.lift(innerIso.eTo)
    override def isIdentity = innerIso.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#Iso1UR[_, _, _] => (this eq i) || (cC == i.cC && eA == i.eA && eB == i.eB)
      case _ => false
    }
  }

  abstract class ThunkIso[A,B](val innerIso: Iso[A,B]) extends Iso1UR[A, B, Thunk] {
    def cC = container[Thunk]
    def from(x: Th[B]) = x.map(innerIso.fromFun)
    def to(x: Th[A]) = x.map(innerIso.toFun)
    lazy val defaultRepTo: Th[B] = Thunk(eB.defaultRepValue)
  }
}

trait ViewsModule extends impl.ViewsDefs { self: Scalan =>
  /**
    * The base type of all isos for user-defined types
    */
  trait EntityIso[From, To] extends IsoUR[From, To] with Product {
    override def canEqual(other: Any) = getClass == other.getClass
    override def equals(other: Any) = other match {
      case i: ViewsModule#EntityIso[_, _] =>
        // Comparing productArity is unnecessary since it should be equal when the classes are equal and
        // in case it isn't, we do little extra work
        (this eq i) || (getClass == i.getClass && productIterator.sameElements(i.productIterator))
      case _ => false
    }
  }

  implicit def viewElement[From, To](implicit iso: Iso[From, To]): Elem[To] = iso.eTo // always ask elem from IsoUR

  trait ViewElem[From, To] extends Elem[To] { _: scala.Equals =>
    def iso: Iso[From, To]

    override protected def _copyWithTypeArgs(argsIterator: Iterator[TypeDesc]): Elem[_] =
      if (typeArgs.isEmpty)
        this
      else {
        // FIXME See https://github.com/scalan/scalan/issues/252
        val isoClass = iso.getClass
        val constructor = getConstructor(isoClass)
        try {
          // -1 because the constructor includes `self` argument, see cachedElem0 below
          val args = argsIterator.take(constructor.getParameterTypes.length - 1).toSeq
          val resultIsoUR = constructor.newInstance(self +: args).asInstanceOf[IsoUR[_, _]]
          // reifyObject in case this iso has been constructed before
          // note that this calculation should always give the same result as "concrete case, call viewElement(*Iso)" in getResultElem: #252
          val resultIso = reifyObject(resultIsoUR)
          resultIso.eTo
        } catch {
          case e: Exception =>
            !!!(s"ViewElem#_copyWithTypeArgs failed (class of iso is ${isoClass.getSimpleName}), override for ${getClass.getSimpleName} may be needed (unless it's a bug in ViewElem)", e)
        }
      }
  }

  object ViewElem {
    def unapply[From, To](ve: ViewElem[From, To]): Option[IsoUR[From, To]] = Some(ve.iso)
  }

  trait ViewElem1[A,From,To,C[_]] extends ViewElem[From, To] { _: scala.Equals =>
    def eItem: Elem[A]
    def cont: Cont[C]
  }

  class ConcreteIsoElem[From, To, IsoType <: IsoUR[From, To]](_eFrom: => Elem[From], _eTo: => Elem[To]) extends IsoURElem[From, To, IsoType]()(_eFrom, _eTo)

  object UnpackableElem {
    private val elems = MutMap.empty[(String, Elem[_]), Option[Iso[_, _]]]
    def unapply(e: Elem[_]) = elems.getOrElseUpdate((currentPass.name, e), {
      val iso = getIsoByElem(e)
      iso match {
        case Def(i: IsoUR[_,_]) =>
          if (i.isIdentity)
            None
          else
            Some(iso)
        case _ => None
      }
    })
  }

  trait IsoBuilder { def apply[T](e: Elem[T]): Iso[_,T] }

  object PairIsos {
    def fromElem[A,B](pe: PairElem[A,B]) = (getIsoByElem(pe.eFst), getIsoByElem(pe.eSnd))

    def unapply[T](e: Elem[T]): Option[(PairElem[_,_], Iso[_,_], Iso[_,_])] = e match {
      case pe: PairElem[a,b] if pe.eFst != UnitElement && pe.eSnd != UnitElement =>
        fromElem(pe) match {
          case (iso1: Iso[s, a] @unchecked, iso2: Iso[t, b] @unchecked) => Some((pe, iso1, iso2))
          case _ => None
        }
      case _ => None
    }
  }

  def getIsoByElem[T](e: Elem[T]): Iso[_, T] = {
    if (e.isInstanceOf[EntityElem[_]] && !shouldUnpack(e)) {
      identityIso(e).asInstanceOf[Iso[_, T]]
    } else if (currentPass.config.shouldUnpackTuples) {
      buildIso(e, new IsoBuilder {
        def apply[S](e: Elem[S]) = {
          val res: Iso[_, S] = e match {
            case PairIsos(_, iso1: Iso[s,a] @unchecked, iso2: Iso[t,b] @unchecked) =>
              if (iso1.isIdentity && iso2.isIdentity) {
                // recursion base (structs)
                val sIso = structToPairIso[s,t,a,b](iso1, iso2)
                val flatIso = flatteningIso(sIso.eFrom)
                flatIso >> sIso.asIso[Struct,S]
              } else {
                val pIso = pairIso(iso1, iso2)
                val deepIso = getIsoByElem(pIso.eFrom)
                deepIso >> pIso.asIso[(s,t),S]
              }
            case _ =>
              getIsoByElem(e)
          }
          res
        }
      })
    } else {
      buildIso(e, new IsoBuilder {
        def apply[S](e: Elem[S]) = {
          val res: Iso[_, S] = e match {
            case PairIsos(_, iso1: Iso[s,a] @unchecked, iso2: Iso[t,b] @unchecked) =>
              if (iso1.isIdentity && iso2.isIdentity) {
                // recursion base
                pairIso(iso1, iso2).asInstanceOf[Iso[_, S]]
              } else {
                getIsoByElem(e)
              }
            case _ =>
              getIsoByElem(e)
          }
          res
        }
      })
    }
  }

  // private[this] val isoCache = MutMap.empty[Elem[_], Iso[_, _]]
  // TODO this caching doesn't work with structs (uncomment and check test("flatteningIso"))
  def buildIso[T](e: Elem[T], builder: IsoBuilder): Iso[_, T] = //isoCache.getOrElseUpdate(
    (//classOf[Iso[_, _]], Seq(e)),
    e match {
      case ie: IsoURElem[_, _, _] =>
        identityIso(ie)
      case ve: ViewElem[_, _] =>
        val eFrom = ve.iso.eFrom
        val deepIso = builder(eFrom)
        if (deepIso.isIdentity)
          ve.iso
        else
          deepIso >> ve.iso
      case pe: PairElem[a,b] => (pe.eFst, pe.eSnd) match {
        case (`UnitElement`, eB) =>
          builder(eB) match { case isoB: Iso[s,b] @unchecked =>
            AbsorbFirstUnitIso(isoB)
          }
        case (eA, `UnitElement`) =>
          builder(eA) match { case isoA: Iso[s,a] @unchecked =>
            AbsorbSecondUnitIso(isoA)
          }
        case (eA, eB) =>
          (builder(eA), builder(eB)) match {
            case (iso1: Iso[s,a] @unchecked, iso2: Iso[t,b] @unchecked) =>
              val pIso = pairIso(iso1,iso2)
              val deepIso = builder(pIso.eFrom)
              deepIso >> pIso
          }
      }
      case pe: SumElem[a,b] =>
        val iso1 = builder(pe.eLeft)
        val iso2 = builder(pe.eRight)
        sumIso(iso1,iso2)
      case fe: FuncElem[a,b] =>
        val iso1 = builder(fe.eDom)
        val iso2 = builder(fe.eRange)
        funcIso(iso1,iso2)
      case ae: ThunkElem[_] =>
        val iso = builder(ae.eItem)
        thunkIso(iso)

      //    case ee1: EntityElem1[_,_,_] =>
      //      val iso = getIsoByElem(ee1.eItem)
      //      TODO implement using ContIso
      case ee: EntityElem[_] =>
        identityIso(ee)
      case be: BaseElem[_] =>
        identityIso(be)
      case se: StructElem[_] =>
        identityIso(se)
      case _ => !!!(s"Don't know how to build iso for element $e")
    }
  ).asInstanceOf[Iso[_,T]]

  def identityIso[A](implicit elem: Elem[A]): Iso[A, A] = IdentityIso[A]()(elem)

  def pairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[(A1, A2), (B1, B2)] =
    PairIso[A1, A2, B1, B2](iso1, iso2)

  def sumIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[A1 | A2, B1 | B2] =
    SumIso[A1, A2, B1, B2](iso1, iso2)

  def composeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Iso[A, C] = {
    if (iso2.isIdentity)
      iso1
    else if (iso1.isIdentity)
      iso2
    else
      (iso2, iso1) match {
        case (Def(iso2d: PairIso[b1, b2, c1, c2]), Def(iso1d: PairIso[a1, a2, _, _])) =>
          val composedIso1 = composeIso(iso2d.iso1, iso1d.iso1.asInstanceOf[Iso[a1, b1]])
          val composedIso2 = composeIso(iso2d.iso2, iso1d.iso2.asInstanceOf[Iso[a2, b2]])
          pairIso(composedIso1, composedIso2)
        case _ => ComposeIso[A, B, C](iso2, iso1)
      }
  }.asInstanceOf[Iso[A, C]]

  def tryComposeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Option[Iso[A, C]] = try {
    val eB1 = iso1.eTo
    val eB2 = iso2.eFrom
    if (eB1 == eB2)
      Some(composeIso(iso2, iso1))
    else {
      val HasConv(conv1) = eB1 -> eB2
      val HasConv(conv2) = eB2 -> eB1
      val iso = converterIso(conv1, conv2)
      Some(iso1 >> iso >> iso2)
    }
  } catch {
    case _: Throwable => None
  }

  def funcIso[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Iso[A => C, B => D] =
    FuncIso[A, B, C, D](iso1, iso2)

  def thunkIso[A,B](iso: Iso[A, B]) = ThunkIso[A, B](iso).asInstanceOf[Iso1[A, B, Thunk]]

  def converterIso[A, B](convTo: Conv[A,B], convFrom: Conv[B,A]): Iso[A,B] = {
    val convToElem = convTo.elem.asInstanceOf[ConverterElem[A, B, _]]
    ConverterIso[A, B](convTo, convFrom)
  }

  def convertBeforeIso[A, B, C](convTo: Conv[A,B], convFrom: Conv[B,A], iso0: Iso[B,C]): Iso[A, C] = composeIso(iso0, converterIso(convTo, convFrom))

  def convertAfterIso[A,B,C](iso0: Iso[A,B], convTo: Conv[B,C], convFrom: Conv[C,B]): Iso[A, C] = composeIso(converterIso(convTo, convFrom), iso0)

  def unifyIsos[A,B,C,D](iso1: Iso[A,C], iso2: Iso[B,D],
                         toD: Conv[C,D], toC: Conv[D,C]): (Iso[A,C], Iso[B,C]) = {
    val ea = iso1.eFrom
    val eb = iso2.eFrom
    implicit val ec = iso1.eTo
    val (i1, i2) =
      if (ec == iso2.eTo)
        (iso1, iso2.asInstanceOf[Iso[B,C]])
      else
        (iso1, convertAfterIso(iso2, toC, toD))
    (i1, i2)
  }
  override protected def getResultElem(receiver: Sym, m: Method, args: List[AnyRef]): Elem[_] = receiver match {
    case Def(iso: IsoUR[_, _]) => m.getName match {
      case "from" => iso.eFrom
      case "to" => iso.eTo
      case "fromFun" => funcElement(iso.eTo, iso.eFrom)
      case "toFun" => funcElement(iso.eFrom, iso.eTo)
      case _ => super.getResultElem(receiver, m, args)
    }
    case _ => super.getResultElem(receiver, m, args)
  }

  type Unpacked[T] = (Rep[Source], Iso[Source, T]) forSome { type Source }
  type UnpackedLambdaResult[T,R] = (Rep[T => R], Iso[Source, R]) forSome { type Source }

  type UnpackTester = Elem[_] => Boolean

  protected val initialUnpackTesters: Set[UnpackTester] = Set.empty
  protected var unpackTesters: Set[UnpackTester] = initialUnpackTesters

  def addUnpackTester(tester: UnpackTester): Unit =
    unpackTesters += tester
  def removeUnpackTester(tester: UnpackTester): Unit =
    unpackTesters -= tester

  def shouldUnpack(e: Elem[_]) = unpackTesters.exists(_(e))

  def defaultUnpackTester(e: Elem[_]) = true //e match { case pe: PairElem[_,_] => false case _ => true }

  object HasViews {
    def unapply[T](s: Exp[T]): Option[Unpacked[T]] =
      unapplyViews(s)
  }

  // for simplifying unapplyViews
  protected def trivialUnapply[T](s: Exp[T]) = (s, identityIso(s.elem))

  def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(d: SLeft[l, r]) =>
      val left = d.left
      val eRight = d.eRight
      (unapplyViews(left), UnpackableElem.unapply(eRight)) match {
        case (None, None) => None
        case (opt1, opt2) =>
          val (sv1, iso1) = opt1.getOrElse(trivialUnapply(left))
          val iso2 = opt2.getOrElse(identityIso(eRight)).asInstanceOf[Iso[_, r]]
          Some((sv1.asLeft(iso2.eFrom), sumIso(iso1, iso2)))
      }
    case Def(d: SRight[l, r]) =>
      val eLeft = d.eLeft
      val right = d.right
      (UnpackableElem.unapply(eLeft), unapplyViews(right)) match {
        case (None, None) => None
        case (opt1, opt2) =>
          val (sv2, iso2) = opt2.getOrElse(trivialUnapply(right))
          val iso1 = opt1.getOrElse(identityIso(eLeft)).asInstanceOf[Iso[_, l]]
          Some((sv2.asRight(iso1.eFrom), sumIso(iso1, iso2)))
      }
    case _ =>
      UnpackableExp.unapply(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  object UnpackableDef {
    def unapply[T](d: Def[T]): Option[Unpacked[T]] =
      d match {
        case view: View[a, T] => Some((view.source, view.iso))
        // TODO make UserTypeDef extend View with lazy iso0/source?
        case _ =>
          None
      }
  }

  object UnpackableExp {
    def unapply[T](e: Exp[T]): Option[Unpacked[T]] =
      e match {
        case Def(UnpackableDef(source, iso: Iso[a, T] @unchecked)) =>
          Some((source.asRep[a], iso))
        case _ =>
          val eT = e.elem
          eT match {
            case UnpackableElem(iso: Iso[a, T] @unchecked) =>
              Some((iso.from(e), iso))
            case _ => None
          }
      }
  }

  object LambdaResultHasViews {
    def unapply[A,C](l: Rep[A => C]): Option[UnpackedLambdaResult[A,C]] = l match {
      case Def(Lambda(_, _, _, HasViews(_, iso: Iso[b, _]))) =>
        Some((l, iso))
      case _ => None
    }
  }

  abstract class View[From, To] extends Def[To] {
    def source: Rep[From]
    def iso: Iso[From, To]
    implicit lazy val selfType = iso.eTo
  }

  case class UnpackView[A, B](view: Rep[B], iso: Iso[A, B]) extends Def[A] {
    implicit def selfType = iso.eFrom
  }

  abstract class View1[A, B, C[_]](val iso: Iso1[A,B,C]) extends View[C[A], C[B]] {
    def innerIso = iso.innerIso
  }

  abstract class View2[A1, A2, B1, B2, C[_, _]](implicit val iso1: Iso[A1, B1], val iso2: Iso[A2, B2]) extends View[C[A1, A2], C[B1, B2]]

  case class PairView[A1, A2, B1, B2](source: Rep[(A1, A2)])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, Tuple2] {
    lazy val iso = pairIso(iso1, iso2)
  }

  case class SumView[A1, A2, B1, B2](source: Rep[A1|A2])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, | ] {
    lazy val iso = sumIso(iso1, iso2)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    // Rule: (V(a, iso1), V(b, iso2)) ==> V((a,b), PairIso(iso1, iso2))
    case Tup(HasViews(a, iso1: Iso[a, c]), HasViews(b, iso2: Iso[b, d])) =>
      PairView((a.asRep[a], b.asRep[b]))(iso1, iso2)

    // Rule: (V(a, iso1), b) ==> V((a,b), PairIso(iso1, id))
    case Tup(HasViews(a, iso1: Iso[a, c]), b: Rep[b]) =>
      PairView((a.asRep[a], b))(iso1, identityIso(b.elem)).self

    // Rule: (a, V(b, iso2)) ==> V((a,b), PairIso(id, iso2))
    case Tup(a: Rep[a], HasViews(b, iso2: Iso[b, d])) =>
      PairView((a, b.asRep[b]))(identityIso(a.elem), iso2).self

    // Rule: V(a, iso1) ; V(b, iso2)) ==> iso2.to(a ; b)
    case block@Semicolon(HasViews(a, iso1: Iso[a, c]), HasViews(b, iso2: Iso[b, d])) =>
      iso2.to(Semicolon(a.asRep[a], b.asRep[b]))

    // Rule: a ; V(b, iso2)) ==> iso2.to(a ; b)
    case block@Semicolon(a: Rep[a], HasViews(b, iso2: Iso[b, d])) =>
      iso2.to(Semicolon(a, b.asRep[b]))

    // Rule: V(a, iso1) ; b ==> a ; b
    case block@Semicolon(HasViews(a, iso1: Iso[a, c]), b: Rep[b]) =>
      Semicolon(a.asRep[a], b)

    // Rule: PairView(source, iso1, _)._1  ==> iso1.to(source._1)
    case First(Def(view@PairView(source))) =>
      view.iso1.to(source._1)

    // Rule: PairView(source, _, iso2)._2  ==> iso2.to(source._2)
    case Second(Def(view@PairView(source))) =>
      view.iso2.to(source._2)

    // Rule: PairView(PairView(source, i2), i1)  ==> PairView(source, PairIso(composeIso(i1.iso1, i2.iso1), composeIso(i1.iso2, i2.iso2)))
    case v1@PairView(Def(v2@PairView(source))) => {
      val pIso1 = composeIso(v1.iso1, v2.iso1)
      val pIso2 = composeIso(v1.iso2, v2.iso2)
      PairView(source)(pIso1, pIso2)
    }

    // Rule: UnpackView(V(source, iso))  ==> source
    case UnpackView(Def(UnpackableDef(source, _)), _) => source

    // Rule: ParExec(nJobs, f @ i => ... V(_, iso)) ==> V(ParExec(nJobs, f >> iso.from), arrayiso(iso))
    //    case ParallelExecute(nJobs:Rep[Int], f@Def(Lambda(_, _, _, HasViews(_, iso: Iso[a, b])))) =>
    //      implicit val ea = iso.eFrom
    //      val parRes = ParallelExecute(nJobs, fun { i => iso.from(f(i)) })(iso.eFrom)
    //      ViewArray(parRes, iso)

    // Rule: loop(V(start, iso), step, isMatch) ==> iso.to(loop(start, iso.to >> step >> iso.from, iso.to >> isMatch))
    case LoopUntil(HasViews(startWithoutViews, iso: Iso[a, b]), step, isMatch) =>
      val start1 = startWithoutViews.asRep[a]
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val step1 = fun { (x: Rep[a]) =>
        val x_viewed = iso.to(x)
        val res_viewed = step.asRep[b => b](x_viewed) // mirrorApply(step.asRep[b => b], x_viewed)
      val res = iso.from(res_viewed)
        res
      }
      val isMatch1 = fun { (x: Rep[a]) =>
        val x_viewed = iso.to(x)
        val res = isMatch.asRep[b => Boolean](x_viewed) // mirrorApply(isMatch.asRep[b => Boolean], x_viewed)
        res
      }
      val loopRes = LoopUntil(start1, step1, isMatch1)
      iso.to(loopRes)

    case _ => super.rewriteDef(d)
  }
}

