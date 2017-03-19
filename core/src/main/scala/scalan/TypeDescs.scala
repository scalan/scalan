package scalan

import scalan.common.Lazy
import scalan.staged.BaseExp
import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.{AnyValManifest, ClassTag}
import scalan.meta.ScalanAst.STpeArg
import scalan.util._

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
    def tag: WeakTypeTag[A]
    final lazy val classTag: ClassTag[A] = ReflectionUtil.typeTagToClassTag(tag)
    // classTag.runtimeClass is cheap, no reason to make it lazy
    final def runtimeClass: Class[_] = classTag.runtimeClass

    def typeArgs: ListMap[String, (TypeDesc, Variance)]
    def typeArgsIterator = typeArgs.valuesIterator.map(_._1)
    final def copyWithTypeArgs(args: Iterator[TypeDesc]) = {
      try {
        val res = _copyWithTypeArgs(args)
        assert(!args.hasNext)
        res
      } catch {
        case e: NoSuchElementException =>
          !!!(s"Not enough elements in the iterator supplied to $this._copyWithTypeArgs", e)
      }
    }
    protected def _copyWithTypeArgs(args: Iterator[TypeDesc]): Elem[_] =
      if (typeArgs.isEmpty)
        this
      else
        cachedElemI(this.getClass, args)
    def mapTypeArgs(f: Elem[_] => Elem[_]) = {
      val newTypeArgs = typeArgsIterator.map {
        case e: Elem[_] =>
          f(e)
        case x => x
      }
      copyWithTypeArgs(newTypeArgs)
    }

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

    def leastUpperBound(e: Elem[_]): Elem[_] =
      commonBound(e, isUpper = true)

    def greatestLowerBound(e: Elem[_]): Elem[_] =
      commonBound(e, isUpper = false)

    /**
      * Helper for leastUpperBound and greatestLowerBound.
      * Should be protected, but then it can't be accessed in subclasses, see
      * http://stackoverflow.com/questions/4621853/protected-members-of-other-instances-in-scala
      */
    def commonBound(e: Elem[_], isUpper: Boolean): Elem[_] = {
      if (this eq e)
        this
      else
        _commonBound(e, isUpper).getOrElse {
          if (isUpper) AnyElement else NothingElement
        }
    }

    // overridden in BaseElem, EntityElem, StructElem
    protected def _commonBound(other: Elem[_], isUpper: Boolean): Option[Elem[_]] = {
      if (this.getClass == other.getClass) {
        // empty type args case is covered too, since copyWithTypeArgs will return `this`
        val newArgs = this.typeArgsIterator.zip(other.typeArgs.valuesIterator).map {
          case (arg1: Elem[_], (arg2: Elem[_], variance)) if variance != Invariant =>
            val isUpper1 = variance match {
              case Covariant => isUpper
              case Contravariant => !isUpper
              case Invariant => !!!("variance can't be Invariant if we got here")
            }
            arg1.commonBound(arg2, isUpper1)
          case (arg1, (arg2, _)) =>
            if (arg1 == arg2)
              arg1
            else {
              // return from the entire _commonBound method! This really throws an exception, but
              // it works since the iterator will be consumed by copyWithTypeArgs below and the
              // exception will get thrown there
              return None
            }
        }
        val newElem = copyWithTypeArgs(newArgs)
        Some(newElem)
      } else
        None
    }

    def <:<(e: Elem[_]) = tag.tpe <:< e.tag.tpe
    def >:>(e: Elem[_]) = e <:< this

    if (isDebug) {
      debug$ElementCounter(this) += 1
    }
  }
  object Elem {
    def unapply[T, E <: Elem[T]](s: Rep[T]): Option[E] = Some(s.elem.asInstanceOf[E])

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

  private[scalan] def getConstructor(clazz: Class[_]) = {
    val constructors = clazz.getDeclaredConstructors()
    if (constructors.length != 1)
      !!!(s"Element class $clazz has ${constructors.length} constructors, 1 expected")
    else
      constructors(0)
  }

  // FIXME See https://github.com/scalan/scalan/issues/252
  def cachedElem[E <: Elem[_]](args: AnyRef*)(implicit tag: ClassTag[E]) = {
    cachedElem0(tag.runtimeClass, None, args).asInstanceOf[E]
  }

  private def cachedElemI(clazz: Class[_], argsIterator: Iterator[TypeDesc]) = {
    val constructor = getConstructor(clazz)
    // -1 because the constructor includes `self` argument, see cachedElem0 below
    val args = argsIterator.take(constructor.getParameterTypes.length - 1).toSeq
    cachedElem0(clazz, Some(constructor), args)
  }

  protected val elemCache = collection.mutable.Map.empty[(Class[_], Seq[AnyRef]), AnyRef]

  private def cachedElem0(clazz: Class[_], optConstructor: Option[java.lang.reflect.Constructor[_]], args: Seq[AnyRef]) = {
    elemCache.getOrElseUpdate(
      (clazz, args), {
        val constructor = optConstructor.getOrElse(getConstructor(clazz))
        val constructorArgs = self +: args
        constructor.newInstance(constructorArgs: _*).asInstanceOf[AnyRef]
      }).asInstanceOf[Elem[_]]
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
    override protected def _commonBound(other: Elem[_], isUpper: Boolean): Option[Elem[_]] =
      if (this == other) Some(this) else None

    private[this] lazy val noTypeArgs =
      if (tag.tpe.typeArgs.isEmpty)
        true
      else
        !!!(s"${getClass.getSimpleName} is a BaseElem for a generic type, must override equals and typeArgs.")
    override def hashCode = tag.tpe.hashCode
  }

  case class PairElem[A, B](eFst: Elem[A], eSnd: Elem[B]) extends Elem[(A, B)] {
    assert(eFst != null && eSnd != null)
    lazy val tag = {
      implicit val tA = eFst.tag
      implicit val tB = eSnd.tag
      weakTypeTag[(A, B)]
    }
    override def getName(f: TypeDesc => String) = s"(${f(eFst)}, ${f(eSnd)})"
    lazy val typeArgs = ListMap("A" -> (eFst -> Covariant), "B" -> (eSnd -> Covariant))
    protected def getDefaultRep = Pair(eFst.defaultRepValue, eSnd.defaultRepValue)
  }

  case class SumElem[A, B](eLeft: Elem[A], eRight: Elem[B]) extends Elem[A | B] {
    lazy val tag = {
      implicit val tA = eLeft.tag
      implicit val tB = eRight.tag
      weakTypeTag[A | B]
    }
    override def getName(f: TypeDesc => String) = s"(${f(eLeft)} | ${f(eRight)})"
    lazy val typeArgs = ListMap("A" -> (eLeft -> Covariant), "B" -> (eRight -> Covariant))
    protected def getDefaultRep = mkLeft[A, B](eLeft.defaultRepValue)(eRight)
  }

  case class FuncElem[A, B](eDom: Elem[A], eRange: Elem[B]) extends Elem[A => B] {
    lazy val tag = {
      implicit val tA = eDom.tag
      implicit val tB = eRange.tag
      weakTypeTag[A => B]
    }
    override def getName(f: TypeDesc => String) = s"${f(eDom)} => ${f(eRange)}"
    lazy val typeArgs = ListMap("A" -> (eDom -> Contravariant), "B" -> (eRange -> Covariant))
    protected def getDefaultRep = {
      val defaultB = eRange.defaultRepValue
      fun[A, B](_ => defaultB)(Lazy(eDom))
    }
  }

  class ArgElem(val tyArg: STpeArg) extends Elem[Any] with Serializable with scala.Equals {
    protected def getDefaultRep = toRep(null.asInstanceOf[Any])(this)
    val tag = ReflectionUtil.createArgTypeTag(tyArg.name).asInstanceOf[WeakTypeTag[Any]]
    def argName = tyArg.name
    lazy val typeArgs = {
      assert(noTypeArgs)
      TypeArgs()
    }
    override protected def _copyWithTypeArgs(args: Iterator[TypeDesc]): Elem[_] = {
      assert(noTypeArgs)
      this
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

    def variance = tyArg.variance
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
  // very ugly casts but should be safe
  val NothingElement: Elem[Nothing] =
    new BaseElem[Null](null)(weakTypeTag[Nothing].asInstanceOf[WeakTypeTag[Null]]).asElem[Nothing]
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
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit def SumElemExtensions[A, B](eAB: Elem[A | B]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

  def TypeArgs(descs: (String, (TypeDesc, Variance))*) = ListMap(descs: _*)

  object TagImplicits {
    implicit def elemToClassTag[A](implicit elem: Elem[A]): ClassTag[A] = elem.classTag
  }

  /** Returns the argument by default, can be overridden for specific types */
  def concretizeElem(e: Elem[_]): Elem[_] = e.mapTypeArgs(concretizeElem)

  // can be removed and replaced with assert(value.elem == elem) after #72
  def assertElem(value: Rep[_], elem: Elem[_]): Unit = assertElem(value, elem, "")
  def assertElem(value: Rep[_], elem: Elem[_], hint: => String): Unit = {
    assert(value.elem == elem,
      s"${value.toStringWithType} doesn't have type ${elem.name}" + (if (hint.isEmpty) "" else s"; $hint"))
  }
  def assertEqualElems[A](e1: Elem[A], e2: Elem[A], m: => String): Unit =
    assert(e1 == e2, s"Element $e1 != $e2: $m")

  def withElemOf[A, R](x: Rep[A])(block: Elem[A] => R): R = block(x.elem)
  def withResultElem[A, B, R](f: Rep[A => B])(block: Elem[B] => R): R = block(withElemOf(f) { e => e.eRange })

  @implicitNotFound(msg = "No Cont available for ${F}.")
  trait Cont[F[_]] extends TypeDesc {
    def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[F[T]]
    def lift[T](implicit eT: Elem[T]): Elem[F[T]]
    def unlift[T](implicit eFT: Elem[F[T]]): Elem[T]
    def getElem[T](fa: Rep[F[T]]): Elem[F[T]]
    def getItemElem[T](fa: Rep[F[T]]): Elem[T] = unlift(getElem(fa))
    def unapply[T](e: Elem[_]): Option[Elem[F[T]]]

    def getName(f: TypeDesc => String): String = {
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
    def map[A,B](a: Rep[F[A]])(f: Rep[A] => Rep[B]): Rep[F[B]]
  }
}

trait TypeDescsExp extends TypeDescs with BaseExp { self: ScalanExp =>
}
