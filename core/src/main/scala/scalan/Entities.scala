package scalan

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import scalan.common.Lazy
import scalan.meta.ScalanAst.{SModuleDef, STraitOrClassDef}
import scalan.util.ReflectionUtil

trait Entities extends TypeDescs { self: Scalan =>
  abstract class EntityElem[A] extends Elem[A] with scala.Equals {
    def parent: Option[Elem[_]]
    val entityName: String = {
      val elemClassSymbol = ReflectionUtil.classToSymbol(this.getClass)
      val n = elemClassSymbol.name.toString.stripSuffix("Elem")
      n
    }
    def convert(x: Rep[Def[_]]): Rep[A] = !!!("should not be called")
    //def getConverterTo[B](eB: Elem[B]): Conv[A,B] = !!!  //TODO make it abstract
    // TODO generate code for this in implementations
    def canEqual(other: Any) = other.isInstanceOf[EntityElem[_]]
    override def equals(other: Any) = other match {
      case other: EntityElem[_] =>
        this.eq(other) ||
          (other.canEqual(this) &&
            this.runtimeClass == other.runtimeClass &&
            this.typeArgsIterator.sameElements(other.typeArgsIterator))
      case _ => false
    }
    override def hashCode = tag.tpe.hashCode
    override protected def _commonBound(other: Elem[_], isUpper: Boolean) = other match {
      case other: EntityElem[_] =>
        val runtimeClass = this.runtimeClass

        if (runtimeClass == other.runtimeClass) {
          // recursion base
          super._commonBound(other, isUpper)
        } else if (isUpper) {
          // Step 1:
          // We find potential common ancestors which have the same class.
          // 1.1: find the ancestor of `other` whose runtime class is a superclass of this (if one exists)
          @tailrec
          def findAncestor(e: EntityElem[_])(pred: Class[_] => Boolean): Option[EntityElem[_]] = {
            if (pred(e.runtimeClass))
              Some(e)
            else
              e.parent match {
                case Some(parent: EntityElem[_]) =>
                  findAncestor(parent)(pred)
                case _ =>
                  None
              }
          }

          for {
            potentialCommonAncestor2 <- findAncestor(other)(_.isAssignableFrom(runtimeClass))
            runtimeClass2 = potentialCommonAncestor2.runtimeClass
            potentialCommonAncestor1 <- findAncestor(EntityElem.this)(_ == runtimeClass2)
          } yield {
            // will hit the recursion base above
            potentialCommonAncestor1.commonBound(potentialCommonAncestor2, isUpper)
          }
        } else {
          // we are looking for lower bound and runtime classes are different
          // just check if one is subtype of another, since we don't know how
          // their type arguments are related

          // TODO support cases like elem[Vector[String]].lowerBound(elem[DenseVector[Object]]) == DenseVector[String]
          if (this <:< other)
            Some(this)
          else if (other <:< this)
            Some(other)
          else
            None
        }
      case _ =>
        None
    }
  }

  abstract class EntityElem1[A, To, C[_]](val eItem: Elem[A], val cont: Cont[C])
    extends EntityElem[To] {
    override def getName(f: TypeDesc => String) = {
      s"${f(cont)}[${f(eItem)}]"
    }
    override def canEqual(other: Any) = other match {
      case _: EntityElem1[_, _, _] => true
      case _ => false
    }
    override def equals(other: Any) = other match {
      case other: EntityElem1[_,_,_] => other.canEqual(this) && cont == other.cont && eItem == other.eItem
      case _ => false
    }
    override def hashCode = eItem.hashCode * 21 + cont.hashCode
  }
  trait ConcreteElem[TData, TClass] extends EntityElem[TClass] with ViewElem[TData, TClass] { eClass =>
    def getConverterFrom[E](eEntity: EntityElem[E]): Option[Conv[E, TClass]] = {
      try {
        val convFun: Rep[E => TClass] =
          fun({ x: Rep[E] => eClass.convert(x.asRep[Def[_]])})(Lazy(eEntity))
        Some(BaseConverter(convFun))
      }
      catch {
        case e: RuntimeException => None
      }
    }
  }
  trait ConcreteElem1[A, TData, TClass, C[_]]
    extends EntityElem1[A, TClass, C]
       with ViewElem1[A, TData, TClass, C] { eClass =>
  }

  implicit class EntityElemExtensions[A <: Def[_]](e: Elem[A]) {
    def asEntityElem = e.asInstanceOf[EntityElem[A]]
  }

  private[this] lazy val modules = mutable.Map.empty[String, SModuleDef]
  def getModules = modules

  def allEntities = modules.values.flatMap(_.allEntities)

  def registerModule(moduleInfo: ModuleInfo) = {
    val m = moduleInfo.moduleDef
    if (modules.contains(m.name))
      !!!(s"Module ${m.name} already registered")
    else {
      modules += (m.name -> m)
    }
  }

  def entityDef(e: EntityElem[_]): STraitOrClassDef = {
    val elemClassSymbol = ReflectionUtil.classToSymbol(e.getClass)
    val moduleName = elemClassSymbol.owner.name.toString.stripSuffix("Defs")
    val module = modules.getOrElse(moduleName, !!!(s"Module $moduleName not found"))
    val entityName = elemClassSymbol.name.toString.stripSuffix("Elem")
    module.allEntities.find(_.name == entityName).getOrElse {
      !!!(s"Entity $entityName not found in module $moduleName")
    }
  }

  def isConcreteElem(e: TypeDesc): Boolean = e match {
    case _: BaseElem[_] =>
      true
    case e: EntityElem[_] if !isConcreteModuloTypeArgs(e) =>
      false
    case e: Elem[_] =>
      e.typeArgsIterator.forall(isConcreteElem)
    case _: Cont[_] => true
  }

  protected def isConcreteModuloTypeArgs(e: EntityElem[_]) = e match {
    case _: ViewElem[_, _] => true
    case _ => false
  }

  implicit class ElemOps[T](e: Elem[T]) {
    def isConcrete = isConcreteElem(e)
    def getDataIso = getIsoByElem(e)

    /**
     * Replaces a root tree of [[PairElem]]s in the given element [[e]] with [[StructElem]]s.
     * All other types are considered as leaves.
     * @return new StructElem if [[e]] is [[PairElem]] otherwise returns [[e]].
     */
    def toStructElemShallow: Elem[_] = e match {
      case pe: PairElem[a,b] =>
        tupleStructElement(pe.eFst.toStructElemShallow, pe.eSnd.toStructElemShallow)
      case _ => e
    }
  }
  trait CompanionElem[T] extends Elem[T] { _: scala.Equals =>
    override def buildTypeArgs = TypeArgs()
    override protected def _copyWithTypeArgs(args: Iterator[TypeDesc]): Elem[_] = this
  }

  trait TypeFamily1[F[_]]
  trait TypeFamily2[F[_, _]]
  trait TypeFamily3[F[_, _, _]]

  trait ConcreteClass0[C]
  trait ConcreteClass1[C[_]]
  trait ConcreteClass2[C[_, _]]
  trait ConcreteClass3[T[_, _, _]]
  trait ConcreteClass4[T[_, _, _, _]]

  implicit class RepDefViewOps[T <: Def[_]](x: Rep[T]) {
    def convertTo[R <: Def[_]](implicit eR: Elem[R]): Rep[R] =
      eR match {
        case entE: EntityElem[R] @unchecked => entE.convert(x)
        case _ => !!!(s"Cannot convert $x to a value of type ${eR.name}: EntityElem expected but ${eR.getClass.getSimpleName} found", x)
      }
  }
}
