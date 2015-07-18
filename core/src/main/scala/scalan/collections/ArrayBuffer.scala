package scalan.collections

import scalan._
import scala.reflect.runtime.universe._

trait ArrayBuffers extends Base { self: Scalan =>
  trait ArrayBuffer[T] {
    def apply(i: Rep[Int]): Rep[T] 
    def length : Rep[Int]
    def map[R:Elem](f: Rep[T] => Rep[R]): Rep[ArrayBuffer[R]]
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
    def fromArray[T: Elem](arr: Arr[T]) = { 
      val buf = empty[T]
      buf ++= arr
    }
  }

  implicit def arrayBufferToArray[T:Elem](buf: Rep[ArrayBuffer[T]]): Arr[T] = buf.toArray

  trait ArrayBufferContainer extends Container[ArrayBuffer] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[ArrayBuffer[T]]
    def lift[T](implicit eT: Elem[T]) = element[ArrayBuffer[T]]
  }

  implicit val arrayBufferFunctor = new Functor[ArrayBuffer] with ArrayBufferContainer {
    def map[A:Elem,B:Elem](xs: Rep[ArrayBuffer[A]])(f: Rep[A] => Rep[B]) = xs.map(f)
  }

  case class ArrayBufferIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, ArrayBuffer](iso) {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    def from(x: Rep[ArrayBuffer[B]]) = x.map(iso.from _)
    def to(x: Rep[ArrayBuffer[A]]) = x.map(iso.to _)
    lazy val defaultRepTo = ArrayBuffer.empty[B]
  }

  case class ArrayBufferElem[A](override val eItem: Elem[A])
    extends EntityElem1[A, ArrayBuffer[A], ArrayBuffer](eItem, container[ArrayBuffer]) {
    override def isEntityType = eItem.isEntityType
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
}

trait ArrayBuffersSeq extends ArrayBuffers { self: ScalanSeq =>
  implicit class SeqArrayBuffer[T](val impl: scala.collection.mutable.ArrayBuffer[T])(implicit val elem: Elem[T]) extends ArrayBuffer[T] {
    def apply(i: Rep[Int]): Rep[T] = impl.apply(i)
    def length : Rep[Int] = impl.length
    def map[R:Elem](f: Rep[T] => Rep[R]): Rep[ArrayBuffer[R]] = new SeqArrayBuffer[R](impl.map(f))
    def update(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = { impl.update(i, v); this }
    def insert(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = { impl.insert(i, v); this }
    def +=(v: Rep[T]): Rep[ArrayBuffer[T]] = { impl += v; this }
    def ++=(a: Arr[T]): Rep[ArrayBuffer[T]] = { impl ++= a; this }
    def remove(i: Rep[Int], n: Rep[Int]): Rep[ArrayBuffer[T]] = { impl.remove(i, n); this }
    def reset():  Rep[ArrayBuffer[T]] = {    impl.clear(); this }  
    def toArray: Arr[T] = impl.toArray(elem.classTag)
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
}

trait ArrayBuffersExp extends ArrayBuffers with ViewsExp { self: ScalanExp =>
  case class ViewArrayBuffer[A, B](source: Rep[ArrayBuffer[A]])(iso: Iso1[A, B, ArrayBuffer])
    extends View1[A, B, ArrayBuffer](iso) {
    //lazy val iso = arrayBufferIso(innerIso)
    def copy(source: Rep[ArrayBuffer[A]]) = ViewArrayBuffer(source)(iso)
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
      val newIso = ArrayBufferIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[ArrayBuffer[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

//  new Iso[ArrayBuffer[A], ArrayBuffer[B]] {
//    lazy val eTo = element[ArrayBuffer[B]]
//    def from(x: Rep[ArrayBuffer[B]]) = x.map(iso.from _)
//    def to(x: Rep[ArrayBuffer[A]]) = x.map(iso.to _)
//    lazy val tag = {
//      implicit val tB = iso.tag
//      weakTypeTag[ArrayBuffer[B]]
//    }
//    lazy val defaultRepTo = ArrayBuffer.empty[B]
//  }
//
//  def arrayBufferIso[A, B](iso: Iso[A, B]): Iso[ArrayBuffer[A], ArrayBuffer[B]] = {
//    implicit val eA = iso.eFrom
//    implicit val eB = iso.eTo
//  }

  protected def hasArrayBufferViewArg(s: Exp[_]): Boolean = s match {
    case Def(_: ViewArrayBuffer[_, _]) => true
    case _ => false
  }

  val HasArrayBufferViewArg = HasArg(hasArrayBufferViewArg)

  def mapUnderlyingArrayBuffer[A,B,C](view: ViewArrayBuffer[A,B], f: Rep[B=>C]): Arr[C] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eC: Elem[C] = f.elem.eRange
    view.source.map { x => f(iso.to(x)) }
  }

  def liftArrayBufferViewFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
    case ArrayBufferApply(Def(view: ViewArrayBuffer[a, b]), i) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = view.innerIso.to(view.source(i))
      Some(res)
    case ArrayBufferAppend(Def(view: ViewArrayBuffer[a, b]), HasViews(value, iso2: Iso[c, d])) if view.innerIso == iso2 =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = ViewArrayBuffer(view.source += value.asRep[a])(view.iso)
      Some(res)
    case ArrayBufferAppendArray(Def(dst: ViewArrayBuffer[a, b]), Def(src: ViewArray[c, d])) if dst.innerIso == src.innerIso =>
      implicit val eA = dst.innerIso.eFrom
      implicit val eB = dst.innerIso.eTo
      val res = ViewArrayBuffer(dst.source ++= src.source.asRep[Array[a]])(ArrayBufferIso(dst.innerIso))
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
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = ViewArray(view.source.toArray)(ArrayIso(view.innerIso))
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
      val bufIso = ArrayBufferIso[a,b](iso)
      val srcBuf = bufIso.from(buf.asRep[ArrayBuffer[b]])
      ViewArrayBuffer(srcBuf += value)(bufIso)
    case mk@MakeArrayBuffer(ctx) =>
       mk.selfType.eItem match {
         case e: ViewElem[a, b] => {
           implicit val eA = e.iso.eFrom
           ViewArrayBuffer(ArrayBuffer.make[a](ctx))(ArrayBufferIso(e.iso))
         }
         case _ => super.rewriteDef(d)
       }
    case ArrayBufferLength(Def(ViewArrayBuffer(xs: ArrBuf[a]))) =>
      val xs1 = xs.asRep[ArrBuf[a]]
      implicit val eA = xs.elem.eItem
      xs.length
    case HasArrayBufferViewArg(_) => liftArrayBufferViewFromArgs(d) match {
      case Some(s) => s
      case _ => super.rewriteDef(d)
    }
    case ArrayBufferMap(xs: Rep[ArrayBuffer[a]] @unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[c, b])))) =>
      val f1 = f.asRep[a => b]
      val xs1 = xs.asRep[ArrayBuffer[a]]
      implicit val eA = xs1.elem.eItem
      implicit val eC = iso.eFrom
      val s = xs1.map { x =>
        val tmp = f1(x)
        iso.from(tmp)
      }
      val res = ViewArrayBuffer(s)(ArrayBufferIso(iso))
      res
    case view1@ViewArrayBuffer(Def(view2@ViewArrayBuffer(arr))) =>
      val compIso = composeIso(view1.innerIso, view2.innerIso)
      implicit val eAB = compIso.eTo
      ViewArrayBuffer(arr)(ArrayBufferIso(compIso))
    case _ =>
      super.rewriteDef(d)
  }

  abstract class ArrayBufferDef[T](implicit val elem: Elem[T]) extends ArrayBuffer[T] with Def[ArrayBuffer[T]] {
    def selfType = element[ArrayBuffer[T]] 
    lazy val uniqueOpId = name(elem)
    
    def apply(i: Rep[Int]): Rep[T] = ArrayBufferApply(this, i)
    def length : Rep[Int] = ArrayBufferLength(this)
    def map[R:Elem](f: Rep[T] => Rep[R]): Rep[ArrayBuffer[R]] = ArrayBufferMap(this, f)
    def update(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferUpdate(this, i, v)
    def insert(i: Rep[Int], v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferInsert(this, i, v)
    def +=(v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferAppend(this, v)
    def ++=(a: Arr[T]): Rep[ArrayBuffer[T]] = ArrayBufferAppendArray(this, a)
    def remove(i: Rep[Int], n: Rep[Int]): Rep[ArrayBuffer[T]] = ArrayBufferRemove(this, i, n)
    def reset():  Rep[ArrayBuffer[T]] = ArrayBufferReset(this)
    def toArray: Arr[T] = ArrayBufferToArray(this)
  }
    
//  def emptyArrayBuffer[T: Elem]: Rep[ArrayBuffer[T]] = ArrayBufferEmpty[T]()
  def emptyArrayBuffer[T: Elem]: Rep[ArrayBuffer[T]] = ArrayBufferUsingFunc(0, fun { i => element[T].defaultRepValue })
  def initArrayBuffer[T: Elem](v: Rep[T]): Rep[ArrayBuffer[T]] = ArrayBufferFromElem(v)
  def makeArrayBuffer[T](name: Rep[String])(implicit e:Elem[T]): Rep[ArrayBuffer[T]] = MakeArrayBuffer(name)(e)
  def createArrayBuffer[T: Elem](count: Rep[Int], f:Rep[Int=>T]): Rep[ArrayBuffer[T]] = ArrayBufferUsingFunc(count, f)

  case class ArrayBufferEmpty[T: Elem]() extends ArrayBufferDef[T] { 
    override def equals(other:Any) = {
      other match {
        case that:ArrayBufferEmpty[_] => (this.selfType equals that.selfType)
        case _ => false
      }
    }
    override def mirror(t:Transformer) = ArrayBufferEmpty[T]()
  }

  case class MakeArrayBuffer[T](ctx: Rep[String])(implicit e:Elem[T]) extends ArrayBufferDef[T] {
    /*
    override def equals(other:Any) = {
      other match {
        case that:MakeArrayBuffer[_] => (this.selfType equals that.selfType) && (this.ctx equals that.ctx)
        case _ => false
      }
    }
    */
    override def mirror(t:Transformer) = MakeArrayBuffer[T](t(ctx))(e)
  }

  case class ArrayBufferFromElem[T: Elem](v: Rep[T]) extends ArrayBufferDef[T] {
    override def mirror(t:Transformer) = ArrayBufferFromElem(t(v))
  }

  case class ArrayBufferUsingFunc[T: Elem](count: Rep[Int], f: Rep[Int=>T]) extends ArrayBufferDef[T] {
    override def mirror(t:Transformer) = ArrayBufferUsingFunc(t(count), t(f))
  }

  case class ArrayBufferApply[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int]) extends BaseDef[T] {
    override def mirror(t: Transformer) = ArrayBufferApply(t(buf), t(i))
    def uniqueOpId = name(selfType)
  }

  case class ArrayBufferLength[T: Elem](buf: Rep[ArrayBuffer[T]]) extends BaseDef[Int] {
    override def mirror(t: Transformer) = ArrayBufferLength(t(buf))
    def uniqueOpId = name(selfType)
  }

  case class ArrayBufferMap[T: Elem, R:Elem](buf: Rep[ArrayBuffer[T]], f: Rep[T => R]) extends ArrayBufferDef[R] { 
    override def mirror(t:Transformer) = ArrayBufferMap(t(buf), t(f))
  }

  case class ArrayBufferUpdate[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int], v: Rep[T]) extends ArrayBufferDef[T] { 
    override def mirror(t:Transformer) = ArrayBufferUpdate(t(buf), t(i), t(v))
  }

  case class ArrayBufferInsert[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int], v: Rep[T]) extends ArrayBufferDef[T] { 
    override def mirror(t:Transformer) = ArrayBufferInsert(t(buf), t(i), t(v))
  }

  case class ArrayBufferAppend[T: Elem](buf: Rep[ArrayBuffer[T]], v: Rep[T]) extends ArrayBufferDef[T] { 
    override def mirror(t:Transformer) = ArrayBufferAppend(t(buf), t(v))
  }

  case class ArrayBufferAppendArray[T: Elem](buf: Rep[ArrayBuffer[T]], a: Arr[T]) extends ArrayBufferDef[T] { 
    override def mirror(t:Transformer) = ArrayBufferAppendArray(t(buf), t(a))
  }

  case class ArrayBufferRemove[T: Elem](buf: Rep[ArrayBuffer[T]], i: Rep[Int], n: Rep[Int]) extends ArrayBufferDef[T] { 
    override def mirror(t:Transformer) = ArrayBufferRemove(t(buf), t(i), t(n))
  }

  case class ArrayBufferReset[T: Elem](buf: Rep[ArrayBuffer[T]]) extends ArrayBufferDef[T] { 
    override def mirror(t:Transformer) = ArrayBufferReset(t(buf))
  }

  case class ArrayBufferToArray[T: Elem](buf: Rep[ArrayBuffer[T]]) extends Def[Array[T]] { 
    def selfType = element[Array[T]]
    def uniqueOpId = name(selfType)
    override def mirror(t:Transformer) = ArrayBufferToArray(t(buf))
  }  

  case class ArrayBufferRep[T: Elem](buf: Rep[ArrayBuffer[T]]) extends ArrayBufferDef[T] {
    override def mirror(t: Transformer) = ArrayBufferRep(t(buf))
  }

  implicit def resolveArrayBuffer[T: Elem](sym: Rep[ArrayBuffer[T]]): ArrayBuffer[T] = sym match  {
    case Def(d: ArrayBufferDef[_]) => d.asInstanceOf[ArrayBuffer[T]]
    case s: Exp[_] => {
      val elem = s.elem
      elem match { 
        case ae: ArrayBufferElem[_] => ArrayBufferRep(sym)(ae.asInstanceOf[ArrayBufferElem[T]].eItem)
	      case _ => ArrayBufferRep(sym)(elem.asInstanceOf[Elem[T]])
      }
    }
    case _ => ???("cannot resolve ReifiableObject for symbol:", sym)
  }

}