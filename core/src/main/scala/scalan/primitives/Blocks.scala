package scalan.primitives

import scala.collection.mutable
import scalan.staged.Expressions
import scalan.{ScalanExp}
import scalan.common.Lazy

trait Blocks extends Expressions { self: ScalanExp =>

  implicit class RepBlock[A](left: Rep[A]) { 
    def |[B](right: Rep[B]) = semicolon(left, right)
  }

  case class Semicolon[A,B](left: Rep[A], right: Rep[B]) extends BaseDef[B]()(right.elem)
  case class SemicolonMulti[B](left: Seq[Rep[_]], right: Rep[B]) extends BaseDef[B]()(right.elem)

  def semicolon[A,B](left: Rep[A], right: Rep[B]): Rep[B] = {
//    implicit val eR = right.elem
    Semicolon(left, right)
  }
  def semicolonMulti[B](xs: Seq[Rep[_]], y: Rep[B]): Rep[B] = {
    val peeled = xs.map(x => peelViews(x))
    val res = peeled.filterNot(isPureDataflow(_))
    SemicolonMulti(res, y)
  }

  def peelViews(x: Rep[_]): Rep[_] = x match {
    case Def(PairView(s)) => peelViews(s)
    case HasViews(s, _) => peelViews(s)
    case _ => x
  }

  def isPureDataflow[A](x: Rep[A]): Boolean = x match {
    case Def(Const(_)) => true
    case _ => false
  }

  object HasSemicolons {
    def unapply(as: Seq[Rep[_]]): Option[Seq[Exp[_]]] = {
      val res = as.filter(a => a match {
        case Def(Semicolon(_,_)) => true
        case Def(SemicolonMulti(_,_)) => true
        case _ => false
      })
      if (res.isEmpty) None else Some(res)
    }
  }

  def addToSet[A](xs: Seq[Rep[A]], y: Rep[A]): Seq[Rep[A]] = {
    if (xs.contains(y)) xs else (xs ++ List(y))
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case Semicolon(a, Def(Semicolon(b,c))) => semicolonMulti(Seq(a,b), c)
    case Semicolon(Def(Semicolon(a,b)), c) => semicolonMulti(Seq(a,b), c)
    case Semicolon(Def(Semicolon(a,b)), Def(Semicolon(c,d))) => semicolonMulti(Seq(a,b,c), d)
    case Semicolon(Def(SemicolonMulti(as,b)), c) =>
      semicolonMulti(addToSet(as.asInstanceOf[Seq[Rep[Any]]], b), c)
    case semi @ SemicolonMulti(HasSemicolons(semicols), d) =>
      val res = mutable.ArrayBuilder.make[Rep[Any]]()
      for (a <- semi.left) {
        if (semicols.contains(a)) {
          a match {
            case Def(Semicolon(b,c)) =>
              res += b
              res += c
            case Def(SemicolonMulti(bs, c)) =>
              for (b <- bs) {
                res += b
                res += c
              }
            // case _ => is covered by HasSemicolons
          }
        }
        else
          res += a
      }
      semicolonMulti(res.result().distinct, d)

    // Rule: as ;; V(b, iso2)) ==> iso2.to(as ; b)
    case block@SemicolonMulti(as, HasViews(b, iso2: Iso[b, d])) =>
      iso2.to(SemicolonMulti(as, b.asRep[b]))

    // WARNING: this should be the last rule in this method
    // Rule: ..V(a, iso).. ;; b ==> ..peelViews(a).. ; b
    case SemicolonMulti(as, b) if shouldUnpackTuples =>
      val peeled = as.map(peelViews(_))
      if (peeled == as)
        super.rewriteDef(d)
      else
        semicolonMulti(peeled, b)

    case _ =>
      super.rewriteDef (d)
  }
}
