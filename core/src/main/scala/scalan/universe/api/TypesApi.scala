package scalan.universe.api

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.Scalan
import scalan.OverloadHack.Overloaded1
import scalan.meta.ScalanAst._
import scalan.meta.PrintExtensions._
import scalan.util.StringUtil
import scalan.util.CollectionUtil._

trait TypesApi { self: Scalan =>
  import UniverseUtils._


  case class QueryParams(checkVariance: Boolean, typeFilter: Elem[_] => Boolean = AllTypes)


  implicit class TypeDescOpsForTypes(t: TypeDesc) {

    def directGens: List[TypeDesc] = List(t)

    def directSpecConstructor(items: List[TypeDesc], constr: List[TypeDesc] => TypeDesc)(implicit params: QueryParams): List[TypeDesc] = {
      val is = genTuples(items)(item => {
        val res = item :: item.directSpecsExclusive
        res.toIterator
      })
      is.diff(items).map(constr).distinct
    }

    def directSpecsExclusive(implicit params: QueryParams): List[TypeDesc] = t match {
      case _: BaseElem[_] => Nil
      case ArgElem(_) => Nil
//      case e: ArrayElem[_] =>
//        directSpecConstructor(
//          List(e.eItem),
//          { case List(a: Elem[_]) => arrayElement(a) }
//          ).diff(List(t))
      case pe: PairElem[_,_] =>
        directSpecConstructor(
          List(pe.eFst, pe.eSnd),
          { case List(a: Elem[_], b: Elem[_]) => pairElement(a,b) }
          ).diff(List(t))
      case pe: SumElem[_,_] =>
        directSpecConstructor(
          List(pe.eLeft, pe.eRight),
          { case List(a: Elem[_], b: Elem[_]) => sumElement(a,b) }
          ).diff(List(t))
      case e: FuncElem[_,_] =>
        val (a, b) = (e.eDom, e.eRange)
        val res = for {
          x <- (if (a.isConcrete) List(a) else Nil) ::: a.directSpecsExclusive
          y <- b.directGens
        } yield funcElement(x.asElem, y.asElem)
        res.distinct
      case e: EntityElem[Def[_]]@unchecked =>
        val argSubst = e.typeArgs.toList
        val ent = Entity(e)
        val argSpecs = genTuples(argSubst) { case (argName, (e, v)) =>
          val a = ent.typeArg(argName)
          val res = (argName, e) :: (if (!params.checkVariance || a.isCovariant) e.allSpecs(params).diff(List(e)).map((argName, _)) else Nil)
          res.iterator
        }
        val entitySpecs = argSpecs.map(as => ent(as.toMap).asElem)

        val specs = for {
          e <- entitySpecs
          ent = Entity(e)
          subEnt <- ent :: ent.subEntitiesIter.toList
          subst <- {
            val parentsWithTypeArgs = subEnt.extendsTypes
            for {
              p <- parentsWithTypeArgs
              subst <- unifyTypes(p, e)
            } yield subst
          }
        } yield subEnt(subst)
        val res = specs.toList.distinct
        res.diff(List(t))
    }

    def allSpecs(implicit params: QueryParams) : List[TypeDesc] =
      t.traverseDepthFirst(_.directSpecsExclusive).distinct

    def isConcrete: Boolean = t match {
      case e: Elem[_] => e.isConcrete
      case _ => false
    }

//    def isClass: Boolean = t match {
//      case EntityApply(e, args) => !e.isTrait
//      case _ => false
//    }
//
    def allConcreteSpecs(implicit params: QueryParams) = {
      val concreteSpecs = allSpecs.filter(_.isConcrete)
      val specs =
        if (t.isConcrete) {
          t :: concreteSpecs // i.e. base types, arrays, or external wrapped types
        }
        else
          concreteSpecs
      specs.distinct
    }

    def isLike(t2: TypeDesc) = unifyTypes(t2, t).isDefined

    def contains(t2: TypeDesc): Boolean = t match {
      case e1: BaseElem[_] => t.isLike(t2)
      case e1: PairElem[_,_] => e1.isLike(t2) || e1.eFst.contains(t2) || e1.eSnd.contains(t2)
      case e1: SumElem[_,_] => e1.isLike(t2) || e1.eLeft.contains(t2) || e1.eRight.contains(t2)
      case e1: FuncElem[_,_] => e1.isLike(t2) || e1.eDom.contains(t2) || e1.eRange.contains(t2)
      case ArgElem(_) => t.isLike(t2)
//      case e: ArrayElem[_] => e.isLike(t2) || e.eItem.contains(t2)
      case e: EntityElem[_] =>
        e.isLike(t2) || e.typeArgs.exists(_._2._1.contains(t2))
//      case TypeApply(_, args) => t.isLike(t2) || args.exists(_.contains(t2))
      case _ => false
    }
  }

  class Entity private (entityDef: SEntityDef, private[TypesApi] val module: SUnitDef) {
    def name = entityDef.name
    def isTrait = entityDef.isTrait
    lazy val typeArgs = entityDef.tpeArgs.map(a => new ArgElem(a))

    lazy val superEntities: Seq[Entity] = {
      val res = for {
        m <- apiModulesIter
        e <- entityDef.getAncestorTraits(m.module).iterator.map(t => Entity(t.name))
      } yield e
      res.toSeq
    }

    lazy val extendsTypes: Seq[TypeDesc] = {
      val env: TypeArgSubst = entityDef.tpeArgs.map(tpeArg => (tpeArg.name, ArgElem(tpeArg))).toMap
      val ancestors = entityDef.ancestors.iterator.filter {
        case STypeApply(STraitCall("Def", List(a)),_) if a.name == this.name => false
        case _ => true
      }
      val types = ancestors.map(a => TypeDesc(a.tpe, env))
      types.toSeq
    }

    val subEntities = mutable.HashMap.empty[String, Entity]

    def addSubEntity(subEnt: Entity) = {
      subEntities += (subEnt.name -> subEnt)
    }

    def subEntitiesIter: Iterator[Entity] = subEntities.values.iterator

    def toTraitCall(args: TypeArgSubst) = {
      val tpeArgs = typeArgs.map(a => args.get(a.argName).map(e => e.tyExpr).getOrElse(a.tyArg.toTraitCall))
      STraitCall(name, tpeArgs)
    }
    def asType: TypeDesc = applySubst(emptySubst)
    def typeArg(name: String): ArgElem = typeArgs.find(a => a.tyArg.name == name).get

    def apply(subst: TypeArgSubst): TypeDesc = applySubst(subst)
    def apply(args: (String,TypeDesc)*): TypeDesc = applySubst(args.toMap)
    def apply(args: TypeDesc*)(implicit o1: Overloaded1) = {
      val subst = (typeArgs.map(_.argName) zip args.toList).toMap
      applySubst(subst)
    }

    def applySubst(subst: TypeArgSubst): TypeDesc = {
      val params = this.typeArgs.map(a => subst.getOrElse(a.argName, a))
      val paramDescs = params.map(p => p.applySubst(subst))
      val descClasses = paramDescs.map {
        case e: Elem[_] => classOf[Elem[_]]
        case c: Cont[_] => classOf[Cont[Any]]
        case d => !!!(s"Unknown type descriptior $d")
      }.toArray

      if (this.isTrait) {
        val methodName = StringUtil.lowerCaseFirst(this.name) + "Element"
        // self.getClass will return the final cake, which should contain the method
        try {
          val method = self.getClass.getMethod(methodName, descClasses: _*)
          try {
            val resultElem = method.invoke(self, paramDescs: _*)
            resultElem.asInstanceOf[Elem[_]]
          } catch {
            case e: Exception =>
              !!!(s"Failed to invoke $methodName($paramDescs)", e)
          }
        } catch {
          case _: NoSuchMethodException =>
            !!!(s"Failed to find element-creating method with name $methodName and parameter classes ${descClasses.map(_.getSimpleName).mkString(", ")}")
        }
      }
      else {
        // concrete case, call viewElement(*Iso)
        val methodName = "iso" + this.name
        try {
          val method = self.getClass.getMethod(methodName, descClasses: _*)
          try {
            val resultIso = method.invoke(self, paramDescs: _*)
            resultIso.asInstanceOf[Iso[_, _]].eTo
          } catch {
            case e: Exception =>
              !!!(s"Failed to invoke $methodName($paramDescs)", e)
          }
        } catch {
          case e: Exception =>
            !!!(s"Failed to find iso-creating method with name $methodName and parameter classes ${descClasses.map(_.getSimpleName).mkString(", ")}")
        }
      }
    }

    override def toString = s"$name[${typeArgs.rep()}]"
    override def equals(other: Any) = other match {
      case other: Entity => module.name == other.module.name && name == other.name
      case _ => false
    }
    override def hashCode = hashCode2(module.name, name)
  }
  object Entity {
    private [TypesApi] def apply(e: SEntityDef, m: SUnitDef) = new Entity(e, m)

    def find(name: String): Option[Entity] = entities.get(name)

    def apply(name: String): Entity =
      find(name).getOrElse(!!!(s"Cannot find Entity $name"))

    def apply[A <: Def[_]](e: Elem[A]): Entity = {
      val ent = e.asEntityElem
      Entity(ent.entityName)
    }
  }


  object BaseType {
    def apply(tyName: String) = STpePrimitives(tyName)
    def apply(tyName: String, args: List[TypeDesc]) = STraitCall(tyName, args.map(_.tyExpr))
  }

//  def mkBaseConstructor1(name: String) = BaseType(STraitCall(name, Nil))
//  def unmkBaseConstructor1(ty: STpeExpr, name: String) = ty match {
//    case STraitCall(`name`, Nil) => Some(())
//    case _ => None
//  }
//
//  def mkExtType(name: String) = BaseType(STraitCall(name, Nil))
//  def unmkExtType(ty: STpeExpr) = ty match {
//    case STraitCall(name, Nil) => Some(name)
//    case _ => None
//  }
//  def mkExtType1(name: String, tyItem: Type) = BaseType(STraitCall(name, List(tyItem.tyExpr)))
//  def unmkExtType1(ty: STpeExpr) = ty match {
//    case STraitCall(name, List(a)) => Some((name, Type(a, Nil)))
//    case _ => None
//  }
//  def mkBaseConstructorType1(name: String, tyItem: Type) = BaseType(STraitCall(name, List(tyItem.tyExpr)))
  def unmkBaseConstructorType1(ty: STpeExpr, name: String) = ty match {
    case STraitCall(`name`, List(a)) => Some(a)
    case _ => None
  }
//  def unmkBaseConstructorTypes(ty: STpeExpr, name: String) = ty match {
//    case STraitCall(`name`, la) => Some(la map { Type(_, Nil)})
//    case _ => None
//  }
//
//  object ArrayType {
//    def apply(tyItem: TypeDesc) = BaseType("Array", List(tyItem))
//    def unapply(ty: STpeExpr): Option[STpeExpr] = unmkBaseConstructorType1(ty, "Array")
//  }
//  object StructType {
//    def apply(tyItems: List[Type]) = BaseType("Struct", tyItems)
//    def unapply(ty: STpeExpr) = unmkBaseConstructorTypes(ty, "Struct")
//    def unapply(t: Type) = unmkBaseConstructorTypes(t.tyExpr, "Struct")
//  }
//  object ArrayCont {
//    def apply() = mkBaseConstructor1("Array")
//    def unapply(ty: STpeExpr) = unmkBaseConstructor1(ty, "Array")
//  }
//  object ListType {
//    def apply(tyItem: Type) = BaseType("List", List(tyItem))
//    def unapply(ty: STpeExpr) = unmkBaseConstructorType1(ty, "List")
//  }
//  object ArrayBufferType {
//    def apply(tyItem: Type) = BaseType("ArrayBuffer", List(tyItem))
//    def unapply(ty: STpeExpr) = unmkBaseConstructorType1(ty, "ArrayBuffer")
//  }

//  class Tuple(val items: List[Type]) extends Type {
//    override def name = s"(${items.rep(_.name)})"
//    override def tyExpr = STpeTuple(items.map(_.tyExpr))
//    override def toDesc(env: Map[ArgElem,TypeDesc]): TypeDesc = {
//      def msg = !!!(s"Don't know how to construce TypeDesc for Tuple($items)")
//      items match {
//        case List(a, b) => (a.toDesc(env), b.toDesc(env)) match {
//          case (ea: Elem[a], eb: Elem[b]) => pairElement(ea, eb)
//          case _ => msg
//        }
//        case h :: t => (h.toDesc(env), Tuple(t).toDesc(env)) match{
//          case (eh: Elem[_], et: Elem[_]) => pairElement(eh, et)
//          case _ => msg
//        }
//      }
//    }
//
//    override def equals(other: Any) = other match {
//      case other: Tuple => items == other.items
//      case _ => false
//    }
//    override def hashCode = items.hashCode
//  }
//  object Tuple {
//    def apply(items: List[Type]) = new Tuple(items)
//    def apply(items: Type*) = new Tuple(items.toList)
//    def unapply(t: Tuple): Option[List[Type]] = Some(t.items)
//  }
//  object Pair {
//    def apply(a: Type, b: Type) = new Tuple(List(a, b))
//    def unapply(t: Tuple): Option[(Type, Type)] = t.items match {
//      case List(a,b) => Some((a, b))
//      case _ => None
//    }
//  }
//
//  class Sum(val items: List[Type]) extends Type {
//    override def name = s"(${items.rep(_.name, " | ")})"
//    override def tyExpr = STraitCall("$bar", items.map(_.tyExpr))
//    override def toDesc(env: Map[ArgElem,TypeDesc]): TypeDesc = {
//      def msg = !!!(s"Don't know how to construce TypeDesc for Sum($items)")
//      items match {
//        case List(a, b) => (a.toDesc(env), b.toDesc(env)) match {
//          case (ea: Elem[_], eb: Elem[_]) => sumElement(ea, eb)
//          case _ => msg
//        }
//        case h :: t => (h.toDesc(env), Tuple(t).toDesc(env)) match{
//          case (eh: Elem[_], et: Elem[_]) => sumElement(eh, et)
//          case _ => msg
//        }
//      }
//    }
//    override def equals(other: Any) = other match {
//      case other: Tuple => items == other.items
//      case _ => false
//    }
//    override def hashCode = items.hashCode
//  }
//  object Sum {
//    def apply(items: List[Type]) = new Sum(items)
//    def unapply(t: Sum): Option[List[Type]] = Some(t.items)
//  }
//
//  class Func(val domain: Type, val range: Type) extends Type {
//    override def name = s"($domain => $range)"
//    override def tyExpr = STpeFunc(domain.tyExpr, range.tyExpr)
//    override def toDesc(env: Map[ArgElem,TypeDesc]): TypeDesc = {
//      def msg = !!!(s"Don't know how to construce TypeDesc for Func($domain, $range)")
//      (domain.toDesc(env), range.toDesc(env)) match {
//        case (ea: Elem[_], eb: Elem[_]) => funcElement(ea, eb)
//        case _ => msg
//      }
//    }
//    override def equals(other: Any) = other match {
//      case other: Func => domain == other.domain && range == other.range
//      case _ => false
//    }
//    override def hashCode = hashCode2(domain, range)
//  }
//  object Func {
//    def apply(domain: Type, range: Type) = new Func(domain, range)
//    def unapply(t: Func): Option[(Type, Type)] = Some(t.domain, t.range)
//  }

  class Module(private[api] val module: SUnitDef) {
    def name = module.name
    def moduleEntitiesIter = module.allEntities.iterator.map(Entity(_, module))
  }

  def apiModulesIter: Iterator[Module] = apiModules.values.toIterator

//  def allEntitiesIter: Iterator[Entity] = entities.values.toIterator

  lazy val apiModules = mutable.HashMap(
    getModules.toIterator.map { case (n, md) => (n, new Module(md)) }.toSeq: _*)

  private var _entities: mutable.HashMap[String, Entity] = _
  def entities = {
    if (_entities == null) {
      _entities = mutable.HashMap(
        apiModules.values.flatMap(_.moduleEntitiesIter.map(e => (e.name, e))).toSeq: _*)
      for (e <- _entities.values) {
        val supers = e.superEntities
        for (s <- supers) s.addSubEntity(e)
      }
    }
    _entities
  }


  def unifyTypeLists(items1: List[TypeDesc], items2: List[TypeDesc]): Option[TypeArgSubst] = {
    val itemsUni = (items1, items2).zipped.map((t1, t2) => unifyTypes(t1,t2))
    if (itemsUni.forall(_.isDefined)) {
      val merge = itemsUni.foldLeft(emptySubst)((acc, s) => acc ++ s.get)
      Some(merge)
    } else
      None
  }

  def unifyTypes(t1: TypeDesc, t2: TypeDesc): Option[TypeArgSubst] = (t1,t2) match {
    case (e1: BaseElem[_], e2: BaseElem[_]) if e1 == e2 => Some(Map())
    case (ta @ ArgElem(_), tb @ ArgElem(_)) =>
      if (ta.argName == tb.argName) Some(Map()) else None
    case (ta @ ArgElem(_), _) => Some(Map(ta.argName -> t2))
    case (e1: EntityElem1[a,_,_], e2: EntityElem1[b,_,_]) if e1.cont == e2.cont =>
      unifyTypes(e1.eItem, e2.eItem)
    case (e1: EntityElem[_], e2: EntityElem[_])
      if e1.entityName == e2.entityName && e1.typeArgs.size == e2.typeArgs.size =>
      unifyTypeLists(e1.typeArgs.values.map(_._1).toList, e2.typeArgs.values.map(_._1).toList)
    case (e1: PairElem[_,_], e2: PairElem[_,_]) =>
      unifyTypeLists(List(e1.eFst, e1.eSnd), List(e2.eFst, e2.eSnd))
    case (e1: SumElem[_,_], e2: SumElem[_,_]) =>
      unifyTypeLists(List(e1.eLeft, e1.eRight), List(e2.eLeft, e2.eRight))
    case (e1: FuncElem[_,_], e2: FuncElem[_,_]) =>
      unifyTypeLists(List(e1.eDom, e1.eRange), List(e2.eDom, e2.eRange))

//    case (EntityApply(name1, args1), EntityApply(name2, args2))
//          if name1 == name2 && args1.length == args2.length =>
//      unifyTypeLists(args1, args2)
    case _ => None
  }

//  implicit class TypeOps(t: Type) {

//  }

//  implicit class TypeListOps(ts: List[Type]) {
//    def asElems: List[Elem[_]] = {
//      val es = ts.map(_.toElem).collect { case Some(e) => e }
//      es
//    }
//  }
}
