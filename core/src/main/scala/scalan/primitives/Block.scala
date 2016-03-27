package scalan.primitives

import scala.collection.mutable
import scalan.staged.Expressions
import scalan.{ScalanExp, ScalanStd, Scalan}
import scalan.common.Lazy

trait Blocks { self: Scalan =>
  def semicolon[A,B](left: Rep[A], right: Rep[B]): Rep[B]
 
  implicit class RepBlock[A](left: Rep[A]) { 
    def |[B](right: Rep[B]) = semicolon(left, right)
  }
}

trait BlocksStd extends Blocks { self: ScalanStd =>
  def semicolon[A,B](left: Rep[A], right: Rep[B]): Rep[B] = right
}

trait BlocksExp extends Blocks with Expressions { self: ScalanExp =>
  case class Semicolon[A,B](left: Exp[A], right: Exp[B])(implicit selfType: Elem[B]) extends BaseDef[B]
  case class SemicolonMulti[B](left: Seq[Exp[_]], right: Exp[B]) extends BaseDef[B]()(right.elem)

  def semicolon[A,B](left: Rep[A], right: Rep[B]): Rep[B] = {
    implicit val eR = right.elem
    Semicolon(left, right)
  }
  def semicolonMulti[B](xs: Seq[Rep[_]], y: Rep[B]): Rep[B] = {
    val res = xs.map(x => peelViews(x))
    SemicolonMulti(res, y)
  }

  def peelViews(x: Rep[_]): Rep[_] = x match {
    case Def(PairView(s)) => peelViews(s)
    case HasViews(s, _) => peelViews(s)
    case _ => x
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
//    case Semicolon(Def(Semicolon(a,b)), Def(Semicolon(c,d))) if b == c => semicolon(Seq(a,b), d)
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
    case block@SemicolonMulti(as, Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
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
