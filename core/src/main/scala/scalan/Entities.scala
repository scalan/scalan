package scalan

import scala.collection.immutable.ListMap
import scala.collection.mutable.{Map => MutMap}
import scala.language.higherKinds
import scalan.common.Lazy
import scalan.meta.ScalanAst.SEntityModuleDef
import scalan.util.ReflectionUtil

trait Entities extends TypeDescs { self: Scalan =>
  abstract class EntityElem[A] extends Elem[A] with scala.Equals {
    def parent: Option[Elem[_]]
    def typeArgs: ListMap[String, TypeDesc]
    def convert(x: Rep[Def[_]]): Rep[A] = !!!("should not be called")
    //def getConverterTo[B](eB: Elem[B]): Conv[A,B] = !!!  //TODO make it abstract
    // TODO generate code for this in implementations
    def canEqual(other: Any) = other.isInstanceOf[EntityElem[_]]
    override def equals(other: Any) = other match {
      case other: EntityElem[_] => other.canEqual(this) && tag.tpe =:= other.tag.tpe
      case _ => false
    }
    override def hashCode = tag.tpe.hashCode
    override protected def getName = {
      val className = runtimeClass.getSimpleName
      if (typeArgs.isEmpty)
        className
      else
        s"$className[${typeArgs.valuesIterator.map(_.name).mkString(", ")}]"
    }
  }

  abstract class EntityElem1[A, To, C[_]](val eItem: Elem[A], val cont: Cont[C])
    extends EntityElem[To] {
    override protected def getName = {
      s"${cont.name}[${eItem.name}]"
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
          fun({ x: Rep[E] => eClass.convert(x.asRep[Def[_]])})(Lazy(eEntity), eClass)
        Some(BaseConverter(convFun)(eEntity, eClass))
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

  private[this] lazy val modules = MutMap.empty[String, SEntityModuleDef]
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

  def entityDef(e: EntityElem[_]) = {
    val elemClassSymbol = ReflectionUtil.classToSymbol(e.getClass)
    val moduleName = elemClassSymbol.owner.name.toString.stripSuffix("Abs")
    val module = modules.getOrElse(moduleName, !!!(s"Module $moduleName not found"))
    val entityName = elemClassSymbol.name.toString.stripSuffix("Elem")
    module.allEntities.find(_.name == entityName).getOrElse {
      !!!(s"Entity $entityName not found in module $moduleName")
    }
  }

  def isConcreteElem[T](e: Elem[T]): Boolean = e match {
    case e: PairElem[_, _] => e.eFst.isConcrete && e.eSnd.isConcrete
    case e: SumElem[_, _] => e.eLeft.isConcrete && e.eRight.isConcrete
    case e: FuncElem[_, _] => e.eDom.isConcrete && e.eRange.isConcrete
    case e: ArrayElem[_] => e.eItem.isConcrete
    case e: ListElem[_] => e.eItem.isConcrete
    case e: ArrayBufferElem[_] => e.eItem.isConcrete
    case _: ViewElem[_,_] => true
    case _: EntityElem[_] => false
    case _: BaseElem[_] => true
    case _ => ???(s"isConcreteElem is not implemented for $e")
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
    override def isEntityType = false
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
