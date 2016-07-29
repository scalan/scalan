package scalan

import scalan.common.Lazy
import scalan.staged.BaseExp
import annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.{AnyValManifest, ClassTag}
import scalan.meta.ScalanAst.STpeArg
import scalan.util.ReflectionUtil

trait TypeDescs extends Base { self: Scalan =>

  sealed trait TypeDesc extends Serializable {
    def getName(f: TypeDesc => String): String
    lazy val name: String = getName(_.name)

    // <> to delimit because: [] is used inside name; {} looks bad with structs.
    override def toString = s"${getClass.getSimpleName}<$name>"
  }

  implicit class TypeDescOps(d: TypeDesc) {
    def asElem[B]: Elem[B] = d.asInstanceOf[Elem[B]]
    def asElemOption[B]: Option[Elem[B]] = if (isElem) Some(d.asInstanceOf[Elem[B]]) else None
    def asCont[C[_]]: Cont[C] = d.asInstanceOf[Cont[C]]
    def asContOption[C[_]]: Option[Cont[C]] = if (isCont) Some(d.asInstanceOf[Cont[C]]) else None
    def isElem: Boolean = d.isInstanceOf[Elem[_]]
    def isCont: Boolean = d.isInstanceOf[Cont[Any] @unchecked]
  }

  type LElem[A] = Lazy[Elem[A]] // lazy element

  /**
    * Reified type representation in Scalan.
    *
    * @tparam A The represented type
    */
  @implicitNotFound(msg = "No Elem available for ${A}.")
  abstract class Elem[A] extends TypeDesc { _: scala.Equals =>
    def isEntityType: Boolean
    def isBaseType: Boolean = this.isInstanceOf[BaseElem[_]]
    def tag: WeakTypeTag[A]
    final lazy val classTag: ClassTag[A] = ReflectionUtil.typeTagToClassTag(tag)
    // classTag.runtimeClass is cheap, no reason to make it lazy
    final def runtimeClass: Class[_] = classTag.runtimeClass
    def typeArgs: ListMap[String, TypeDesc]
    def typeArgsIterator = typeArgs.valuesIterator
    // should only be called by defaultRepValue
    protected def getDefaultRep: Rep[A]
    lazy val defaultRepValue = getDefaultRep

    override def getName(f: TypeDesc => String) = {
      import ClassTag._
      val className = classTag match {
        case _: AnyValManifest[_] | Any | AnyVal | AnyRef | Object | Nothing | Null => classTag.toString
        case objectTag => objectTag.runtimeClass.getSimpleName
      }
      if (typeArgs.isEmpty)
        className
      else {
        val typeArgString = typeArgsIterator.map(f).mkString(", ")
        s"$className[$typeArgString]"
      }
    }

    def <:<(e: Elem[_]) = tag.tpe <:< e.tag.tpe
    def >:>(e: Elem[_]) = e <:< this

    if (isDebug) {
      debug$ElementCounter(this) += 1
    }
  }
  object Elem {
    def unapply[T, E <: Elem[T]](s: Rep[T]): Option[E] = Some(rep_getElem(s).asInstanceOf[E])

    def pairify(es: Iterator[Elem[_]]): Elem[_] = {
      def step(a: Elem[_], b: Elem[_], tail: Iterator[Elem[_]]): Elem[_] = {
        if (tail.hasNext) {
          val c = tail.next()
          pairElement(a, step(b, c, tail))
        }
        else {
          pairElement(a, b)
        }
      }
      val a = es.next()
      val b = es.next()
      step(a, b, es)
    }
  }

  private val debug$ElementCounter = counter[Elem[_]]

  protected val elemCache = collection.mutable.Map.empty[(Class[_], Seq[AnyRef]), AnyRef]

  def cachedElem[E <: Elem[_]](args: AnyRef*)(implicit tag: ClassTag[E]) = {
    val clazz = tag.runtimeClass
    elemCache.getOrElseUpdate(
      (clazz, args), {
        val constructors = clazz.getDeclaredConstructors()
        if (constructors.length != 1) {
          !!!(s"Element class $clazz has ${constructors.length} constructors, 1 expected")
        } else {
          val constructor = constructors(0)
          val constructorArgs = self +: args
          constructor.newInstance(constructorArgs: _*).asInstanceOf[Elem[_]]
        }
      }).asInstanceOf[E]
  }

  def cleanUpTypeName(tpe: Type) = tpe.toString.
    replaceAll("[A-Za-z0-9_.]*this.", "").
    replace("scala.math.Numeric$", "").
    replace("scala.math.Ordering$", "").
    replace("scala.", "").
    replace("java.lang.", "").
    replaceAll("""[^# \[\],>]*[#$]""", "")

  def element[A](implicit ea: Elem[A]): Elem[A] = ea

  class BaseElem[A](defaultValue: A)(implicit val tag: WeakTypeTag[A]) extends Elem[A] with Serializable with scala.Equals {
    protected def getDefaultRep = toRep(defaultValue)(this)
    override def isEntityType = false
    override def canEqual(other: Any) = other.isInstanceOf[BaseElem[_]]
    override def equals(other: Any) = other match {
      case other: BaseElem[_] =>
        this.eq(other) ||
          (other.canEqual(this) &&
            this.runtimeClass == other.runtimeClass &&
            noTypeArgs)
      case _ => false
    }
    lazy val typeArgs = {
      assert(noTypeArgs)
      TypeArgs()
    }
    private[this] lazy val noTypeArgs =
      if (tag.tpe.typeArgs.isEmpty)
        true
      else
        !!!(s"${getClass.getSimpleName} is a BaseElem for a generic type, must override equals and typeArgs.")
    override def hashCode = tag.tpe.hashCode
  }

  case class PairElem[A, B](eFst: Elem[A], eSnd: Elem[B]) extends Elem[(A, B)] {
    assert(eFst != null && eSnd != null)
    override def isEntityType = eFst.isEntityType || eSnd.isEntityType
    lazy val tag = {
      implicit val tA = eFst.tag
      implicit val tB = eSnd.tag
      weakTypeTag[(A, B)]
    }
    override def getName(f: TypeDesc => String) = s"(${f(eFst)}, ${f(eSnd)})"
    lazy val typeArgs = ListMap("A" -> eFst, "B" -> eSnd)
    protected def getDefaultRep = Pair(eFst.defaultRepValue, eSnd.defaultRepValue)
  }

  case class SumElem[A, B](eLeft: Elem[A], eRight: Elem[B]) extends Elem[A | B] {
    override def isEntityType = eLeft.isEntityType || eRight.isEntityType
    lazy val tag = {
      implicit val tA = eLeft.tag
      implicit val tB = eRight.tag
      weakTypeTag[A | B]
    }
    override def getName(f: TypeDesc => String) = s"(${f(eLeft)} | ${f(eRight)})"
    lazy val typeArgs = ListMap("A" -> eLeft, "B" -> eRight)
    protected def getDefaultRep = mkLeft[A, B](eLeft.defaultRepValue)(eRight)
  }

  case class FuncElem[A, B](eDom: Elem[A], eRange: Elem[B]) extends Elem[A => B] {
    override def isEntityType = eDom.isEntityType || eRange.isEntityType
    lazy val tag = {
      implicit val tA = eDom.tag
      implicit val tB = eRange.tag
      weakTypeTag[A => B]
    }
    override def getName(f: TypeDesc => String) = s"${f(eDom)} => ${f(eRange)}"
    lazy val typeArgs = ListMap("A" -> eDom, "B" -> eRange)
    protected def getDefaultRep = {
      val defaultB = eRange.defaultRepValue
      fun[A, B](_ => defaultB)(Lazy(eDom), eRange)
    }
  }

  class ArgElem(val tyArg: STpeArg) extends Elem[Any] with Serializable with scala.Equals {
    protected def getDefaultRep = toRep(Default.OfAny.value)(this)
    val tag = ReflectionUtil.createArgTypeTag(tyArg.name).asInstanceOf[WeakTypeTag[Any]]
    override def isEntityType = false
    lazy val typeArgs = {
      assert(noTypeArgs)
      TypeArgs()
    }
    private[this] lazy val noTypeArgs =
      if (tag.tpe.typeArgs.isEmpty)
        true
      else
        !!!(s"${getClass.getSimpleName} is a ArgElem for a generic type, must override equals and typeArgs.")

    override def getName(f: TypeDesc => String) = {
      if (typeArgs.isEmpty)
        tyArg.name
      else {
        val typeArgString = typeArgsIterator.map(f).mkString(", ")
        s"${tyArg.name}[$typeArgString]"
      }
    }

    def isCovariant = tyArg.isCovariant
    def tyExpr = tyArg.toTraitCall
    def toDesc(env: Map[ArgElem,TypeDesc]): TypeDesc = env.get(this) match {
      case Some(d) => d
      case None =>
        !!!(s"Don't know how to convert ArgElem $this to TypeDesc")
    }

    override def <:<(e: Elem[_]) = if (this == e) true else super.<:<(e)

    override def canEqual(other: Any) = other.isInstanceOf[ArgElem]
    override def equals(other: Any) = other match {
      case other: ArgElem =>
        this.eq(other) ||
          (other.canEqual(this) && this.tyArg == other.tyArg)
      case _ => false
    }
    override def hashCode = tag.tpe.hashCode
  }
  object ArgElem {
    def apply(name: String): ArgElem = new ArgElem(STpeArg(name, None, Nil))
    def apply(a: STpeArg) = new ArgElem(a)
    def unapply(t: ArgElem): Option[STpeArg] = Some(t.tyArg)
  }

  val AnyElement: Elem[Any] = new BaseElem[Any](null)
  val AnyRefElement: Elem[AnyRef] = new BaseElem[AnyRef](null)
  implicit val BooleanElement: Elem[Boolean] = new BaseElem(false)
  implicit val ByteElement: Elem[Byte] = new BaseElem(0.toByte)
  implicit val ShortElement: Elem[Short] = new BaseElem(0.toShort)
  implicit val IntElement: Elem[Int] = new BaseElem(0)
  implicit val LongElement: Elem[Long] = new BaseElem(0L)
  implicit val FloatElement: Elem[Float] = new BaseElem(0.0F)
  implicit val DoubleElement: Elem[Double] = new BaseElem(0.0)
  implicit val UnitElement: Elem[Unit] = new BaseElem(())
  implicit val StringElement: Elem[String] = new BaseElem("")
  implicit val CharElement: Elem[Char] = new BaseElem('\u0000')

  implicit def pairElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A, B)] =
    cachedElem[PairElem[A, B]](ea, eb)
  implicit def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A | B] =
    cachedElem[SumElem[A, B]](ea, eb)
  implicit def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B] =
    cachedElem[FuncElem[A, B]](ea, eb)
  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] =
    cachedElem[ScalaArrayElem[A]](eA)
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit def SumElemExtensions[A, B](eAB: Elem[A | B]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]
  implicit def ArrayElemExtensions[A](eArr: Elem[Array[A]]): ArrayElem[A] = eArr.asInstanceOf[ArrayElem[A]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

  def TypeArgs(descs: (String, TypeDesc)*) = ListMap(descs: _*)

  object TagImplicits {
    implicit def elemToClassTag[A](implicit elem: Elem[A]): ClassTag[A] = elem.classTag
  }

  def elemFromRep[A](x: Rep[A])(implicit eA: Elem[A]): Elem[A] = eA match {
    case ve: ViewElem[_,_] =>
      x.asRep[Def[_]].selfType1.asInstanceOf[Elem[A]]
    case pe: PairElem[a,b] =>
      implicit val ea = pe.eFst
      implicit val eb = pe.eSnd
      val pair = x.asRep[(a, b)]
      pairElement(elemFromRep(pair._1)(ea), elemFromRep(pair._2)(eb))
    case _ => eA
  }

  def concretizeElem(e: Elem[_]): Elem[_] = e match {
    case e: BaseElem[_] => e
    case e: ArrayElem[_] => arrayElement(concretizeElem(e.eItem))
    case e: ArrayBufferElem[_] => arrayBufferElement(concretizeElem(e.eItem))
    case e: PairElem[_,_] => pairElement(concretizeElem(e.eFst), concretizeElem(e.eSnd))
    case e: SumElem[_,_] => sumElement(concretizeElem(e.eLeft), concretizeElem(e.eRight))
    case _ => e
  }

  // can be removed and replaced with assert(value.elem == elem) after #72
  def assertElem(value: Rep[_], elem: Elem[_]): Unit = assertElem(value, elem, "")
  def assertElem(value: Rep[_], elem: Elem[_], hint: => String): Unit

  def assertEqualElems[A](e1: Elem[A], e2: Elem[A], m: => String) =
    assert(e1 == e2, s"Element $e1 != $e2: $m")

  @implicitNotFound(msg = "No Cont available for ${F}.")
  trait Cont[F[_]] extends TypeDesc {
    def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[F[T]]
    def lift[T](implicit eT: Elem[T]): Elem[F[T]]
    def unlift[T](implicit eFT: Elem[F[T]]): Elem[T]
    def getElem[T](fa: Rep[F[T]]): Elem[F[T]]
    def getItemElem[T](fa: Rep[F[T]]): Elem[T] = unlift(getElem(fa))
    def unapply[T](e: Elem[_]): Option[Elem[F[T]]]

    def getName(f: TypeDesc => String) = {
      // note: will use WeakTypeTag[x], so x type parameter ends up in the result
      // instead of the actual type parameter it's called with (see below)
      def tpeA[x] = tag[x].tpe

      val tpe = tpeA[Nothing]

      val str = cleanUpTypeName(tpe)

      if (str.endsWith("[x]"))
        str.stripSuffix("[x]")
      else
        "[x]" + str
    }
    def isFunctor = this.isInstanceOf[Functor[F]]
  }

  def container[F[_]: Cont] = implicitly[Cont[F]]

  implicit def containerElem[F[_]:Cont, A:Elem]: Elem[F[A]] = container[F].lift(element[A])

  trait Functor[F[_]] extends Cont[F] {
    def map[A:Elem,B:Elem](a: Rep[F[A]])(f: Rep[A] => Rep[B]): Rep[F[B]]
  }
}

trait TypeDescsStd extends TypeDescs { self: ScalanStd =>
  override def assertElem(value: Rep[_], elem: Elem[_], hint: => String) =
    (value, elem) match {
      case (_: Boolean, BooleanElement) | (_: Byte, ByteElement) | (_: Short, ShortElement) |
           (_: Int, IntElement) | (_: Long, LongElement) | (_: Float, FloatElement) |
           (_: Double, DoubleElement) | ((), UnitElement) | (_: Char, CharElement) => // do nothing
      case _ =>
        assert(elem.runtimeClass.isInstance(value),
          s"$value has class ${value.getClass}, expected ${elem.runtimeClass}" + (if (hint.isEmpty) "" else s"; $hint"))
    }
}

trait TypeDescsExp extends TypeDescs with BaseExp { self: ScalanExp =>

  override def assertElem(value: Rep[_], elem: Elem[_], hint: => String) =
    assert(value.elem == elem,
      s"${value.toStringWithType} doesn't have type ${elem.name}" + (if (hint.isEmpty) "" else s"; $hint"))

  def withElemOf[A, R](x: Rep[A])(block: Elem[A] => R) = block(x.elem)
  def withResultElem[A, B, R](f: Rep[A => B])(block: Elem[B] => R) = block(withElemOf(f) { e => e.eRange })

  //  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
  //    case ee: ElemElem[a] => ElemDef[a](x)
  //    case _ => super.toRep(x)(eA)
  //  }
}
