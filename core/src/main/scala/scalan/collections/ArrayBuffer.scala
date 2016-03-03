package scalan.collections

import scalan._
import scala.reflect.runtime.universe._

trait ArrayBuffers extends Base { self: Scalan =>
  trait ArrayBuffer[T] {
    implicit val eItem: Elem[T]
    def apply(i: Rep[Int]): Rep[T] 
    def length : Rep[Int]
    def mapBy[R: Elem](f: Rep[T => R]): Rep[ArrayBuffer[R]]
    def map[R: Elem](f: Rep[T] => Rep[R]): Rep[ArrayBuffer[R]] = mapBy(fun(f))
    def update(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]]
    def insert(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]]
    def +=(v: Rep[T]): Rep[ArrayBuffer[T]]
    def ++=(a: Arr[T]): Rep[ArrayBuffer[T]]
    def remove(i: Rep[Int], n: Rep[Int]): Rep[ArrayBuffer[T]]
    def reset():  Rep[ArrayBuffer[T]]     
    def toArray: Arr[T]
  }

  type ArrBuf[T] = Rep[ArrayBuffer[T]]

  object ArrayBuffer { 
    def apply[T: Elem](v: Rep[T]) = initArrayBuffer[T](v)
    def create[T: Elem](count: Rep[Int], f:Rep[Int]=>Rep[T]) = createArrayBuffer(count, f)
    def make[T](name: Rep[String])(implicit e:Elem[T]) = makeArrayBuffer[T](name)(e)
    def empty[T: Elem] = emptyArrayBuffer[T]
    def fromArray[T: Elem](arr: Arr[T]) = createArrayBufferFromArray(arr)
//    {
//      val buf = empty[T]
//      buf ++= arr
//    }
  }

  implicit val arrayBufferFunctor = new Functor[ArrayBuffer] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[ArrayBuffer[T]]
    def lift[T](implicit eT: Elem[T]) = element[ArrayBuffer[T]]
    def unlift[T](implicit eFT: Elem[ArrayBuffer[T]]) = eFT.eItem
    def getElem[T](fa: Rep[ArrayBuffer[T]]) = !!!("Operation is not supported by ArrayBuffer container " + fa)
    def unapply[T](e: Elem[_]) = e match {
      case e: ArrayBufferElem[_] => Some(e.asElem[ArrayBuffer[T]])
      case _ => None
    }
    def map[A:Elem,B:Elem](xs: Rep[ArrayBuffer[A]])(f: Rep[A] => Rep[B]) = xs.mapBy(fun(f))
  }

  case class ArrayBufferElem[A](override val eItem: Elem[A])
    extends EntityElem1[A, ArrayBuffer[A], ArrayBuffer](eItem, container[ArrayBuffer]) {
    def parent: Option[Elem[_]] = None
    override def isEntityType = eItem.isEntityType
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eItem))
    }
    lazy val tag = {
      implicit val tag1 = eItem.tag
      weakTypeTag[ArrayBuffer[A]]
    }
    protected def getDefaultRep = ArrayBuffer.empty[A](eItem)
  }

  implicit def arrayBufferElement[A](implicit eItem: Elem[A]): Elem[ArrayBuffer[A]] = new ArrayBufferElem[A](eItem)
  implicit def ArrayBufferElemExtensions[A](eArr: Elem[ArrayBuffer[A]]): ArrayBufferElem[A] = eArr.asInstanceOf[ArrayBufferElem[A]]

  implicit def resolveArrayBuffer[T: Elem](buf: Rep[ArrayBuffer[T]]): ArrayBuffer[T]

  def emptyArrayBuffer[T: Elem]: Rep[ArrayBuffer[T]]
  def initArrayBuffer[T: Elem](v: Rep[T]): Rep[ArrayBuffer[T]]
  def makeArrayBuffer[T](name: Rep[String])(implicit e:Elem[T]): Rep[ArrayBuffer[T]]
  def createArrayBuffer[T: Elem](count: Rep[Int], f:Rep[Int=>T]): Rep[ArrayBuffer[T]]
  def createArrayBufferFromArray[T: Elem](arr: Arr[T]): Rep[ArrayBuffer[T]]
}

trait ArrayBuffersStd extends ArrayBuffers { self: ScalanStd =>
  implicit class SeqArrayBuffer[T](val impl: scala.collection.mutable.ArrayBuffer[T])(implicit val eItem: Elem[T]) extends ArrayBuffer[T] {
    def apply(i: Rep[Int]): Rep[T] = impl.apply(i)
    def length : Rep[Int] = impl.length
    def mapBy[R: Elem](f: Rep[T => R]): Rep[ArrayBuffer[R]] = new SeqArrayBuffer[R](impl.map(f))
    def update(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = { impl.update(i, v); this }
    def insert(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = { impl.insert(i, v); this }
    def +=(v: Rep[T]): Rep[ArrayBuffer[T]] = { impl += v; this }
    def ++=(a: Arr[T]): Rep[ArrayBuffer[T]] = { impl ++= a; this }
    def remove(i: Rep[Int], n: Rep[Int]): Rep[ArrayBuffer[T]] = { impl.remove(i, n); this }
    def reset():  Rep[ArrayBuffer[T]] = { impl.clear(); this }
    def toArray: Arr[T] = impl.toArray(eItem.classTag)
  }  

  implicit def resolveArrayBuffer[T: Elem](buf: Rep[ArrayBuffer[T]]): ArrayBuffer[T] = buf

  def emptyArrayBuffer[T: Elem]: Rep[ArrayBuffer[T]] = scala.collection.mutable.ArrayBuffer.empty[T]
  def initArrayBuffer[T: Elem](v: Rep[T]): Rep[ArrayBuffer[T]] = scala.collection.mutable.ArrayBuffer(v)
  def makeArrayBuffer[T](name: Rep[String])(implicit e:Elem[T]): Rep[ArrayBuffer[T]] = scala.collection.mutable.ArrayBuffer.empty[T]
  def createArrayBuffer[T: Elem](count: Rep[Int], f:Rep[Int=>T]): Rep[ArrayBuffer[T]] = {
    val buf = scala.collection.mutable.ArrayBuffer.empty[T]
    for (i <- 0 until count) {
      buf += f(i)
    }
    buf
  }
  def createArrayBufferFromArray[T: Elem](arr: Arr[T]): Rep[ArrayBuffer[T]] = {
    val buf = scala.collection.mutable.ArrayBuffer.empty[T]
    buf ++= arr
    buf
  }
}

trait ArrayBuffersExp extends ArrayBuffers with ViewsDslExp { self: ScalanExp =>
  case class ViewArrayBuffer[A, B](source: Rep[ArrayBuffer[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, ArrayBuffer](arrayBufferIso(innerIso)) {
    override def toString = s"ViewArrayBuffer[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewArrayBuffer[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object UserTypeArrayBuffer {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case pae: ArrayBufferElem[_] =>
          pae.eItem match {
            case e: ViewElem[_, _] => Some(e.iso)
            case _ => None
          }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewArrayBuffer[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeArrayBuffer(iso: Iso[a, b]) =>
      val newIso = arrayBufferIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[ArrayBuffer[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]


  protected def hasArrayBufferViewArg(s: Exp[_]): Boolean = s match {
    case Def(_: ViewArrayBuffer[_, _]) => true
    case _ => false
  }

  val HasArrayBufferViewArg = HasArg(hasArrayBufferViewArg)

  def mapUnderlyingArrayBuffer[A,B,C](view: ViewArrayBuffer[A,B], f: Rep[B=>C]): Rep[ArrayBuffer[C]] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eC: Elem[C] = f.elem.eRange
    ArrayBuffer.fromArray(view.source.toArray.mapBy(iso.toFun >> f))
  }

  def liftArrayBufferViewFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
    case ArrayBufferApply(Def(view: ViewArrayBuffer[a, b]), i) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = view.innerIso.to(view.source(i))
      Some(res)
    case ArrayBufferAppend(Def(view: ViewArrayBuffer[a, b]), HasViews(value, iso2: Iso[c, d])) if view.innerIso == iso2 =>
      val innerIso: Iso[a, b] = view.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eB = innerIso.eTo
      val res = ViewArrayBuffer(view.source += value.asRep[a], innerIso)
      Some(res)
    case ArrayBufferAppendArray(Def(dst: ViewArrayBuffer[a, b]), Def(src: ViewArray[c, d])) if dst.innerIso == src.innerIso =>
      val innerIso = dst.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eB = innerIso.eTo
      val res = ViewArrayBuffer(dst.source ++= src.source.asRep[Array[a]], innerIso)
      Some(res)
      /*
    case ArrayBufferAppend(Def(view: ViewArrayBuffer[a, b]), Def(UnpackableDef(value, iso2: Iso[c, d]))) if view.innerIso == iso2 =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = ViewArrayBuffer(view.source += value.asRep[a])(view.innerIso)
      Some(res)
      */
    case ArrayBufferMap(Def(view: ViewArrayBuffer[_, _]), f) =>
      Some(mapUnderlyingArrayBuffer(view, f))
    case ArrayBufferToArray(Def(view: ViewArrayBuffer[a, b])) =>
      val innerIso = view.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eB = innerIso.eTo
      val res = ViewArray(view.source.toArray, innerIso)
      Some(res)
    case _ => None
  }

  override def rewriteDef[T](d: Def[T]) = d match {
     /*
    case ArrayBufferAppend(HasViews(buf, iso1: Iso[a, b]), HasViews(value, iso2: Iso[c, d]))  =>
      implicit val eA = iso1.eFrom
      implicit val eB = iso1.eTo
      val buf_ = buf.asRep[ArrayBuffer[a]]
      val value_ = value.asRep[a]
      ViewArrayBuffer(buf_ += value_)(iso1)
      */
    case ArrayBufferAppend(buf, HasViews(srcValue, iso: Iso[a,b]))  =>
      val value = srcValue.asRep[a]
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val bufIso = arrayBufferIso[a,b](iso)
      val srcBuf = bufIso.from(buf.asRep[ArrayBuffer[b]])
      ViewArrayBuffer(srcBuf += value, iso)
    case mk@MakeArrayBuffer(ctx) =>
       mk.selfType.eItem match {
         case e: ViewElem[a, b] => {
           implicit val eA = e.iso.eFrom
           ViewArrayBuffer(ArrayBuffer.make[a](ctx), e.iso)
         }
         case _ => super.rewriteDef(d)
       }
    case ArrayBufferLength(Def(ViewArrayBuffer(xs: ArrBuf[a], _))) =>
      val xs1 = xs.asRep[ArrBuf[a]]
      implicit val eA = xs.elem.eItem
      xs.length
    case HasArrayBufferViewArg(_) => liftArrayBufferViewFromArgs(d) match {
      case Some(s) => s
      case _ => super.rewriteDef(d)
    }
    case ArrayBufferMap(xs, Def(IdentityLambda())) => xs
    case ArrayBufferMap(xs: Rep[ArrayBuffer[a]] @unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[c, b])))) =>
      val f1 = f.asRep[a => b]
      val xs1 = xs.asRep[ArrayBuffer[a]]
      implicit val eA = xs1.elem.eItem
      implicit val eC = iso.eFrom
      val s = xs1.mapBy(f1 >> iso.fromFun)
      ViewArrayBuffer(s, iso)
    case view1@ViewArrayBuffer(Def(view2@ViewArrayBuffer(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewArrayBuffer(arr, compIso)
    case _ =>
      super.rewriteDef(d)
  }

  abstract class ArrayBufferDef[T](implicit val eItem: Elem[T]) extends ArrayBuffer[T] with Def[ArrayBuffer[T]] {
    lazy val selfType = element[ArrayBuffer[T]]

    def apply(i: Rep[Int]): Rep[T] = ArrayBufferApply(this, i)
    def length : Rep[Int] = ArrayBufferLength(this)
    def mapBy[R: Elem](f: Rep[T => R]): Rep[ArrayBuffer[R]] = ArrayBufferMap(this, f)
    def update(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferUpdate(this, i, v)
    def insert(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferInsert(this, i, v)
    def +=(v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferAppend(this, v)
    def ++=(a: Arr[T]): Rep[ArrayBuffer[T]] = ArrayBufferAppendArray(this, a)
    def remove(i: Rep[Int], n: Rep[Int]): Rep[ArrayBuffer[T]] = ArrayBufferRemove(this, i, n)
    def reset():  Rep[ArrayBuffer[T]] = ArrayBufferReset(this)
    def toArray: Arr[T] = ArrayBufferToArray(this)
  }

  def emptyArrayBuffer[T: Elem]: Rep[ArrayBuffer[T]] = reflectMutable(ArrayBufferEmpty[T]())
  def initArrayBuffer[T: Elem](v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferFromElem(v)
  def makeArrayBuffer[T](name: Rep[String])(implicit e:Elem[T]): Rep[ArrayBuffer[T]] = MakeArrayBuffer(name)(e)
  def createArrayBuffer[T: Elem](count: Rep[Int], f:Rep[Int=>T]): Rep[ArrayBuffer[T]] = ArrayBufferUsingFunc(count, f)
  def createArrayBufferFromArray[T: Elem](arr: Arr[T]): Rep[ArrayBuffer[T]] = ArrayBufferFromArray(arr)

  case class ArrayBufferEmpty[T: Elem]() extends ArrayBufferDef[T] { 
    override def equals(other:Any) = {
      other match {
        case that:ArrayBufferEmpty[_] => (this.selfType equals that.selfType)
        case _ => false
      }
    }
  }

  case class MakeArrayBuffer[T](ctx: Rep[String])(implicit e: Elem[T]) extends ArrayBufferDef[T]

  case class ArrayBufferFromElem[T: Elem](v: Rep[T]) extends ArrayBufferDef[T]

  case class ArrayBufferUsingFunc[T: Elem](count: Rep[Int], f: Rep[Int=>T]) extends ArrayBufferDef[T]

  case class ArrayBufferApply[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int]) extends BaseDef[T]

  case class ArrayBufferLength[T](buf: Rep[ArrayBuffer[T]])(implicit val eT: Elem[T]) extends BaseDef[Int]

  case class ArrayBufferMap[T, R](buf: Rep[ArrayBuffer[T]], f: Rep[T => R])(implicit val eT: Elem[T], eR: Elem[R]) extends ArrayBufferDef[R]

  case class ArrayBufferUpdate[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int], v: Rep[T]) extends ArrayBufferDef[T]

  case class ArrayBufferInsert[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int], v: Rep[T]) extends ArrayBufferDef[T]

  case class ArrayBufferAppend[T: Elem](buf: Rep[ArrayBuffer[T]], v: Rep[T]) extends ArrayBufferDef[T]

  case class ArrayBufferAppendArray[T: Elem](buf: Rep[ArrayBuffer[T]], a: Arr[T]) extends ArrayBufferDef[T]

  case class ArrayBufferRemove[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int], n: Rep[Int]) extends ArrayBufferDef[T]

  case class ArrayBufferReset[T: Elem](buf: Rep[ArrayBuffer[T]]) extends ArrayBufferDef[T]

  case class ArrayBufferToArray[T: Elem](buf: Rep[ArrayBuffer[T]]) extends ArrayDef[T]

  case class ArrayBufferFromArray[T: Elem](arr: Rep[Array[T]]) extends ArrayBufferDef[T]

  case class ArrayBufferRep[T: Elem](buf: Rep[ArrayBuffer[T]]) extends ArrayBufferDef[T]

  implicit def resolveArrayBuffer[T: Elem](sym: Rep[ArrayBuffer[T]]): ArrayBuffer[T] = sym match  {
    case Def(d: ArrayBufferDef[_]) => d.asInstanceOf[ArrayBuffer[T]]
    case s: Exp[_] => {
      val elem = s.elem
      elem match { 
        case ae: ArrayBufferElem[_] => ArrayBufferRep(sym)(ae.asInstanceOf[ArrayBufferElem[T]].eItem)
        case _ =>
          !!!(s"Type mismatch: expected ArrayBufferElem but was $elem", sym)
      }
    }
    case _ => ???("cannot resolve ArrayBuffer", sym)
  }

}
