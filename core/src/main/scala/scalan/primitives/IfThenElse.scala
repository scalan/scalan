package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan.Base
import scalan.Scalan

trait IfThenElse extends Base with Effects { self: Scalan =>

  def IF(cond: Rep[Boolean]): IfBranch = new IfBranch(cond)

  class IfBranch(cond: Rep[Boolean]) {
    def apply[T](thenp: => Rep[T]) = THEN(thenp)

    def THEN[T](thenp: => Rep[T]) = new ThenBranch[T](cond, thenp)
  }

  class ElseIfBranch[T](cond: Rep[Boolean], outer: ThenBranch[T]) {
    def apply(thenp: => Rep[T]) = THEN(thenp)

    def THEN(thenp: => Rep[T]) = new ThenBranch[T](cond, thenp) {
      override def ELSE(elsep: => Rep[T]) = outer.elseIf(cond, thenp, elsep)
    }
  }

  class ThenBranch[T](cond: Rep[Boolean], thenp: => Rep[T]) {
    def ELSE(elsep: => Rep[T]): Rep[T] = ifThenElse(cond, thenp, elsep)

    def elseIf(cond1: => Rep[Boolean], thenp1: => Rep[T], elsep1: => Rep[T]) =
      ELSE(ifThenElse(cond1, thenp1, elsep1))

    def ELSEIF(cond1: => Rep[Boolean]) = new ElseIfBranch[T](cond1, this)
  }

  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T] {
    lazy val selfType = thenp.elem.leastUpperBound(elsep.elem).asElem[T]
  }

  def reifyBranch[T](b: => Exp[T]): Exp[T] = {
    val Block(res) = reifyEffects(b)
    res
  }

  def ifThenElse[T](cond: Exp[Boolean], thenp: => Exp[T], elsep: => Exp[T]): Exp[T] = {
    val t = reifyBranch(thenp)
    val e = reifyBranch(elsep)
    IfThenElse(cond, t, e)
  }

  implicit class IfThenElseOps[T](tableEntry: TableEntry[T]) {
    def isIfThenElse = tableEntry.rhs match { case IfThenElse(_,_,_) => true case _ => false }
  }
  
  override def aliasSyms(e: Any): List[Exp[Any]] = e match {
    case IfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Exp[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Exp[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Exp[Any]] = e match {
    case IfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }


  override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case IfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }

  override def boundSyms(e: Any): List[Exp[Any]] = e match {
    case IfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }

  override def effectSyms(x: Any): List[Exp[Any]] = x match {
    case IfThenElse(c, t, e) =>
      val ts = Def.unapply(t).map(d => effectSyms(d)).toList.flatten
      val es = Def.unapply(e).map(d => effectSyms(d)).toList.flatten
      ts ::: es
    case _ => super.effectSyms(x)
  }

  def liftFromIfThenElse[A,B,C](cond: Rep[Boolean], a: Rep[A], b: Rep[B], iso1: Iso[A,C], iso2: Iso[B,C]): Rep[C] = {
    assertEqualElems(iso1.eTo, iso2.eTo, s"liftFromIfThenElse($cond, $a, $b, $iso1, $iso2)")
    val ea = iso1.eFrom
    val eb = iso2.eFrom
    implicit val ec = iso1.eTo
    val source = IF (cond) THEN { mkLeft(a)(eb) } ELSE { mkRight(b)(ea) }
    val res = SumView(source)(iso1, iso2).self.joinSum
    res
  }

  def liftFromIfThenElse[A,B,C,D](
        cond: Rep[Boolean], a: Rep[A], b: Rep[B],
        iso1: Iso[A,C], iso2: Iso[B,D],
        toD: Conv[C,D], toC: Conv[D,C]): Rep[C] =
  {
    val ea = iso1.eFrom
    val eb = iso2.eFrom
    implicit val ec = iso1.eTo
    val (i1, i2) = unifyIsos(iso1, iso2, toD, toC)
    liftFromIfThenElse(cond, a, b, i1, i2)
  }

  type UnpackedIf[A,B,C,D] = (Rep[Boolean], Rep[A], Rep[B], Iso[A,C], Iso[B,D], Conv[C,D], Conv[D,C])

  object IfThenElseHasViewsWithConvertibleBranches {
    def unapply[T](d: Def[T]): Option[UnpackedIf[A,B,C,D] forSome {type A; type B; type C; type D}] =
    {
      val optHasViews = d match {
        case ite @ IfThenElse(cond, HasViews(a, iso1: Iso[a, c]), HasViews(b, iso2: Iso[b, d])) =>
          Some((ite.cond, a, b, iso1, iso2))

        case ite @ IfThenElse(cond, a, HasViews(b, iso2: Iso[b, d])) =>
          Some((ite.cond, a, b, identityIso(a.elem), iso2))

        case ite @ IfThenElse(cond, HasViews(a, iso1: Iso[a, c]), b) =>
          Some((ite.cond, a, b, iso1, identityIso(b.elem)))

        case _ => None
      }
      optHasViews match {
        case Some((c, a, b, iso1: Iso[a,c], iso2: Iso[b,d])) =>
          val eC = iso1.eTo
          val eD = iso2.eTo
          if (eC == eD || (eC.isConcrete && eD.isConcrete))
            (eC, eD) match {
              case IsConvertible(cTo, cFrom) =>
                Some((c, a.asRep[a], b.asRep[b], iso1, iso2, cTo, cFrom))
              case _ => None
            }
          else
            None
        case _ => None
      }
    }
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    // Rule: if (true) t else e ==> t
    case IfThenElse(Def(Const(true)), t, _) => t

    // Rule: if (false) t else e ==> e
    case IfThenElse(Def(Const(false)), _, e) => e

    // Rule: if (c) t else t  ==> t
    case IfThenElse(c, t, e) if t == e => t

    // Rule: if (!c) t else e ==> if (c) e else t
    case IfThenElse(Def(ApplyUnOp(not, c: Rep[Boolean @unchecked])), t, e) if not == Not => ifThenElse(c, e, t)

    // Rule: if (c) V(a, iso1) else V(b, iso2) when IsConvertible(iso1.eTo, iso2.eTo) ==>
    case IfThenElseHasViewsWithConvertibleBranches(
           cond, thenp, elsep, iso1: Iso[a, c], iso2: Iso[b, d], cTo, cFrom) =>
      val c = cond
      val t = thenp.asRep[a]
      val e = elsep.asRep[b]
      liftFromIfThenElse(c, t, e, iso1, iso2, cTo.asRep[Converter[c,d]], cFrom.asRep[Converter[d,c]])

    // Rule: (if (c1) t1 else e1, if (c2) t2 else e2) when c1 == c2 ==> if (c1) (t1, t2) else (e1, e2)
    case Tup(Def(IfThenElse(c1, t1, e1)), Def(IfThenElse(c2, t2, e2))) if c1 == c2 =>
      IF (c1) THEN { Pair(t1, t2) } ELSE { Pair(e1, e2) }

    // These two rules are commented now. Seem to be inefficient (see test37pairIf test in LmsSmokeItTests )
    /*
    // Rule: (if (c) t else e)._1 ==> if (c) t._1 else e._1
    case First(Def(IfThenElse(cond, thenp: Rep[(a, b)] @unchecked, elsep))) =>
      implicit val (eA, eB) = (thenp.elem.eFst, thenp.elem.eSnd)
      IfThenElse[a](cond, First(thenp), First(elsep.asRep[(a, b)]))

    // Rule: (if (c) t else e)._2 ==> if (c) t._2 else e._2
    case Second(Def(IfThenElse(cond, thenp: Rep[(a, b)] @unchecked, elsep))) =>
      implicit val (eA, eB) = (thenp.elem.eFst, thenp.elem.eSnd)
      IfThenElse[b](cond, Second(thenp), Second(elsep.asRep[(a, b)]))
    */

    // Rule: (if (c) t else e)(arg) ==> if (c) t(arg) else e(arg)
    case apply: Apply[a, b] => apply.f match {
      case Def(IfThenElse(c, t, e)) =>
        IF (c) { t.asRep[a=>b](apply.arg) } ELSE { e.asRep[a=>b](apply.arg) }
      case _ => super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }
}
