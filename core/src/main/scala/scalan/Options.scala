package scalan

import scala.{Left => L, Right => R}

trait Options extends Views { self: Scalan =>

//  object Opt {
//    class IsoOption[A: Elem] extends IsoBase[(Unit | A), Option[A]] {
//      implicit lazy val optionRepDefault = defaultVal[Rep[Option[A]]](toRep(Option.empty[A])(eTo))
//      def manifest = {
//        implicit val mA = element[A].manifest
//        Predef.manifest[Option[A]]
//      }
//
//      override def from = (o: Option[A]) => o match {
//        case Some(a) => R(a)
//        case _ => L(())
//      }
//
//      override def to = (x: (Unit | A)) => x match {
//        case L(_) => None
//        case R(a) => Some(a)
//      }
//    }
//  }
//
//  implicit def isoOption[A: Elem]: Iso[(Unit | A), Option[A]]
//  def some[A:Elem](x: Rep[A]): Rep[Option[A]]
}

trait OptionsSeq extends Options with ViewsSeq { self: ScalanSeq =>
//  implicit def isoOption[A:Elem]: Iso[(Unit|A), Option[A]] = new Opt.IsoOption[A] {
//    override def fromStaged = x => this.from(x)
//    override def toStaged = x => this.to(x)
//  }
//  def some[A:Elem](x: Rep[A]): Rep[Option[A]] = Option(x)
}

trait OptionsExp extends Options with ProxyExp { self: ScalanStaged =>

//  abstract class ExpOption[A:Elem] extends Def[Option[A]]
//  case class ExpSome[A:Elem](x: Rep[A]) extends ExpOption[A]
//  case class ExpNone[A:Elem]() extends ExpOption[A]
//
//  trait OptionOps[A] {
//    implicit def eA: Elem[A]
//    def isEmpty(implicit e:Elem[Boolean]): Rep[Boolean]
//    def get(implicit e:Elem[A]): Rep[A]
//    def getOrElse[B >: A](default: Rep[B])(implicit eB: Elem[B]): Rep[B]
//    def toRight[L:Elem](left: => L): Rep[(L|A)] =
//      IF (isEmpty) { Left[L,A](left) } ELSE { Right[L,A](get) }
//  }
//  implicit def repToOptionProxyOps[A:Elem](p: Rep[Option[A]]): OptionOps[A] = {
//    implicit val mA = element[A].manifest;
//    proxyOps[Option[A], OptionOps[A]](p)
//  }
//
//  object ExpOption {
//    class IsoExpOption[A:Elem] extends Opt.IsoOption[A] {
//      override def fromStaged = (p: Rep[Option[A]]) => p.toRight(())
//      override def toStaged = (p: Rep[(Unit|A)]) => p.fold(u => ExpNone[A](), a => ExpSome[A](a))
//    }
////    addRewriteRules({
////      case MethodCall(Def(ExpSome(x)), "get", _) => x
////      case MethodCall(Def(ExpSome(_)), "isEmpty", _) => false
////      case MethodCall(Def(ExpNone()), "isEmpty", _) => true
////      case MethodCall(Def(ExpSome(x)), "getOrElse", _) => x
////      case MethodCall(Def(ExpNone()), "getOrElse", default :: _) => default
////    })
//  }
//
//  implicit def isoOption[A:Elem]: Iso[(Unit|A), Option[A]] = new ExpOption.IsoExpOption[A]
//  def some[A:Elem](x: Rep[A]): Rep[Option[A]] = ExpSome(x)
}
