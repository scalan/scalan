package scalan.compilation.lms

import scala.reflect.{classTag, ClassTag, SourceContext}
import scala.reflect.runtime.universe._

import scalan.compilation.Passes
import scalan.util.{ReflectionUtil, StringUtil}

trait LmsBridge extends Passes {
  val lms: LmsBackend

  import scalan._

  type LmsFunction = (lms.Exp[A] => lms.Exp[B]) forSome {type A; type B}

  class LmsMirror private (
       lastExp: Option[lms.Exp[_]],
       private val symMirror: Map[Exp[_], lms.Exp[_]],
       funcMirror: Map[Exp[_], LmsFunction],
       private val symMirrorInsertionOrder: List[Exp[_]])
  {
    def addSym(scalanExp: Exp[_], lmsExp: lms.Exp[_]) = {
      if (isDebug) {
        println(s"${scalanExp.toStringWithDefinition} -> ${lms.toStringWithDefinition(lmsExp)}")
      }
      new LmsMirror(Some(lmsExp), symMirror.updated(scalanExp, lmsExp), funcMirror, scalanExp :: symMirrorInsertionOrder)
    }
    def addFuncAndSym(scalanExp: Exp[_], lmsFunc: LmsFunction, lmsExp: lms.Exp[_]) =
      new LmsMirror(Some(lmsExp), symMirror.updated(scalanExp, lmsExp), funcMirror.updated(scalanExp, lmsFunc), scalanExp :: symMirrorInsertionOrder)
    def addFunc(scalanExp: Exp[_], lmsFunc: LmsFunction) =
      new LmsMirror(lastExp, symMirror, funcMirror.updated(scalanExp, lmsFunc), symMirrorInsertionOrder)
    private def withoutLastExp = new LmsMirror(None, symMirror, funcMirror, symMirrorInsertionOrder)
    private def withLastExp(e: lms.Exp[_]) = new LmsMirror(Some(e), symMirror, funcMirror, symMirrorInsertionOrder)
    private def lastExpOrElse(default: => lms.Exp[_]) = lastExp.getOrElse(default)

    def symMirror[A](scalanExp: Exp[_]): lms.Exp[A] = symMirror.apply(scalanExp).asInstanceOf[lms.Exp[A]]
    def symsMirror[A](scalanExps: List[Exp[_]]): List[lms.Sym[A]] =
      scalanExps.map(e => symMirror[A](e).asInstanceOf[lms.Sym[A]])
    def symMirrorUntyped(scalanExp: Exp[_]): lms.Exp[_] = symMirror.apply(scalanExp)
    def funcMirror[A, B](scalanExp: Exp[_]): lms.Exp[A] => lms.Exp[B] =
      funcMirror.apply(scalanExp).asInstanceOf[lms.Exp[A] => lms.Exp[B]]
    def funcMirrorUntyped(scalanExp: Exp[_]): LmsFunction = funcMirror.apply(scalanExp)

    def summaryMirror(ss: Summary): lms.Summary =
      new lms.Summary(ss.maySimple, ss.mstSimple, ss.mayGlobal, ss.mstGlobal, ss.resAlloc, ss.control,
                      symsMirror(ss.mayRead), symsMirror(ss.mstRead), symsMirror(ss.mayWrite), symsMirror(ss.mstWrite))

    def mirrorLambda[I, R](lam: Lambda[I, R]): (lms.Exp[I] => lms.Exp[R]) = {
      val lamX = lam.x
      val f = { x: lms.Exp[I] =>
        val sched = lam.filterReifyRoots(lam.scheduleSingleLevel)
        val finalMirror = addSym(lamX, x).mirrorDefs(lam, sched)
        val res = finalMirror.lastExpOrElse(x)
        res.asInstanceOf[lms.Exp[R]]
      }
      f
    }

    def mirrorMonoid[A](monoid: RepMonoid[A]): (LmsMirror, lms.Exp[A], lms.Exp[(A, A)] => lms.Exp[A]) = {
      monoid.append match {
        case opSym@Def(lambda: Lambda[(A, A), A] @unchecked) =>
          val op = mirrorLambda(lambda)
          // TODO insert if not yet in the mirror
          val zero = symMirror[A](monoid.zero)
          (/*addSym(monoid.zero, zero).*/addFunc(opSym, op), zero, op)
      }
    }

    def mirrorBlock[R](block: ThunkDef[_], dflt: Rep[_]): () => lms.Exp[R] = { () =>
      val sched = block.filterReifyRoots(block.scheduleSingleLevel)
      val finalMirror = mirrorDefs(block, sched)
      val res = finalMirror.lastExpOrElse(symMirrorUntyped(dflt))
      res.asInstanceOf[lms.Exp[R]]
    }

    def mirrorDefs(fromGraph: AstGraph, defs: Seq[TableEntry[_]]): LmsMirror = {
      val finalMirror = defs.foldLeft(withoutLastExp) { (m, t) =>
        val s = t.sym
        m.symMirror.get(s) match {
          case Some(lmsExp) => m.withLastExp(lmsExp)
          case None =>
            val d = t.rhs
            try {
              transformDef(m, fromGraph, s, d)
            } catch {
              case e: Exception => !!!(s"Failed to transform ${s.toStringWithType} = $d\nCurrent mirror state: $m", e)
            }
        }
      }
      finalMirror
    }

    override def toString = {
      val symMirrorString = symMirrorInsertionOrder.reverseIterator.map {
        e => s"${e.toStringWithDefinition} -> ${symMirrorUntyped(e)}"
      }.mkString(",\n  ")
      val funcMirrorString = funcMirror.keys.mkString(", ")
      s"LmsMirror{symbols: $symMirrorString;\nfunctions: $funcMirrorString}"
    }
  }

  object LmsMirror {
    val empty = new LmsMirror(None, Map.empty, Map.empty, Nil)
  }

  /**
   * Finds the [[lms]] method name corresponding to Scalan primitive. Default implementation
   * splits CamelCase class name into segments, lowercases first and second one and inserts _
   * between them. E.g. <c>lmsMethodName(_: ListToArray[_]) == "list_toArray"</c>.
   */
  protected def lmsMethodName(d: Def[_], primitiveName: String) = {
    val parts = primitiveName.split("(?<=.)(?=\\p{Lu})", 2)
    parts.map(StringUtil.lowerCaseFirst).mkString("_")
  }

  case class ReflectedPrimitive(lmsMethodMirror: MethodMirror, paramFieldMirrors: List[FieldMirror], areParamsFunctions: List[Boolean], needsSourceContext: Boolean)

  private[this] lazy val lmsTpe =
    ReflectionUtil.classToSymbol(lms.getClass).toType
  // mirror in the scala-reflect sense, not the class LmsMirror sense
  protected lazy val lmsMirror =
    runtimeMirror(lms.getClass.getClassLoader).reflect(lms)
  private[this] lazy val selfTypeSym =
    ReflectionUtil.classToSymbol(classOf[BaseDef[_]]).toType.decl(TermName("selfType")).asTerm
  private[this] lazy val FunctionSym = typeOf[_ => _].typeSymbol

  protected def lmsMemberByName(name: String) = lmsTpe.member(TermName(name))

  protected def reflectPrimitive(clazz: Class[_], d: Def[_]) = {
    // should be guaranteed by the call context, uncomment to verify
    // assert(clazz.isInstance(d.asInstanceOf[AnyRef]))
    val instanceMirror = runtimeMirror(clazz.getClassLoader).reflect(d)

    val fieldMirrors = ReflectionUtil.paramFieldMirrors(clazz, instanceMirror, selfTypeSym)
    val paramsLength = fieldMirrors.length

    val lmsMethodName = this.lmsMethodName(d, clazz.getSimpleName)
    val lmsMethod = lmsMemberByName(lmsMethodName) match {
      case NoSymbol =>
        !!!(s"LMS method $lmsMethodName not found")
      case t: TermSymbol if t.isOverloaded =>
        val alternatives = t.alternatives.collect {
          case m: MethodSymbol if m.paramLists.map(_.length).sum == paramsLength => m
        }
        alternatives match {
          case List(method) => method
          case _ => !!!(s"Multiple LMS method overloads with name $lmsMethodName and $paramsLength total parameters found")
        }
      case m: MethodSymbol => m
    }

    val lmsMethodMirror = lmsMirror.reflectMethod(lmsMethod)

    // assume LMS method has at least one parameter list, and the
    // last of them is non-empty
    val needsSourceContext = {
      val lastParam = lmsMethod.paramLists.last.last.asTerm
      lastParam.typeSignature =:= typeOf[SourceContext]
    }

    // need to filter out SourceContext parameter
    val lmsParamsWithoutSC = {
      val lmsParams = lmsMethod.paramLists.flatten
      if (needsSourceContext) lmsParams.init else lmsParams
    }

    val areParamsFunctions = for (param <- lmsParamsWithoutSC) yield {
      param.asTerm.typeSignature match {
        // we only check it's a function here, could check argument/result types if necessary
        case TypeRef(_, FunctionSym, _) => true
        case _ => false
      }
    }

    ReflectedPrimitive(lmsMethodMirror, fieldMirrors, areParamsFunctions, needsSourceContext)
  }

  private[this] val primitives = collection.mutable.Map.empty[Class[_], ReflectedPrimitive]

  protected def mapParam(m: LmsMirror, x: Any, isFunction: Boolean): Any = x match {
    case e: Exp[_] => if (isFunction) m.funcMirrorUntyped(e) else m.symMirrorUntyped(e)
    case elem: Element[_] => elemToManifest(elem)
    case seq: Seq[_] => seq.map(mapParam(m, _, isFunction))
    case arr: Array[_] => arr.map(mapParam(m, _, isFunction))
    case x => x
  }

  /**
   * Extracts the arguments from d. Default implementation returns the list of constructor parameters,
   * including implicit ones. Can be overridden if the LMS method expects the arguments in a different
   * amount or order (Scalan [[Exp]] will be translated to LMS and [[Element]] will be translated to
   * [[Manifest]] automatically).
   */
  protected def extractParams(d: Def[_], fieldMirrors: List[FieldMirror]): List[Any] =
    extractParamsByReflection(d, fieldMirrors)

  private[this] def extractParamsByReflection(d: Any, fieldMirrors: List[FieldMirror]): List[Any] =
    fieldMirrors.map(_.bind(d).get)

  /**
   * Inserts a Scalan [[Def]] into an [[LmsMirror]].
   *
   * The default implementation assumes [[lms]] has a method with
   * the name corresponding to the Scalan primitive case class name
   * which takes arguments in the same order (with Elements replaced
   * by Manifests). This should be overridden for any primitives
   * this doesn't apply to.
   */
  protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]): LmsMirror = {
    val clazz = d.getClass

    val ReflectedPrimitive(lmsMethodMirror, paramFieldMirrors, areParamsFunctions, needsSourceContext) =
      primitives.getOrElseUpdate(clazz, reflectPrimitive(clazz, d))

    val scalanParams = extractParams(d, paramFieldMirrors)

    val lmsParams = scalanParams.zip(areParamsFunctions).map { case (param, isFunction) =>
      mapParam(m, param, isFunction)
    } ++ (if (needsSourceContext) List(implicitly[SourceContext]) else Nil)

    val lmsExp = lmsMethodMirror.apply(lmsParams: _*).
      asInstanceOf[lms.Exp[_]]

    m.addSym(sym, lmsExp)
  }

  // can't just return lmsFunc: lms.Exp[A] => lms.Exp[B], since mirrorDefs needs to be called in LMS context
  def apply[A, B](g: PGraph) = { x: lms.Exp[A] =>
    val finalMirror = LmsMirror.empty.mirrorDefs(g, g.schedule)
    val lmsFunc = finalMirror.funcMirror[A, B](g.roots.last)
    lmsFunc(x)
  }

  case class ReflectedElement(clazz: Class[_], fieldMirrors: List[FieldMirror])
  private[this] val elements = collection.mutable.Map.empty[Class[_], ReflectedElement]
  private[this] val elementClassTranslations = collection.mutable.Map.empty[Class[_], Class[_]]

  def registerElemClass[E: ClassTag, C: ClassTag] =
    elementClassTranslations += (classTag[E].runtimeClass -> classTag[C].runtimeClass)

  private[this] lazy val eItemSym =
    ReflectionUtil.classToSymbol(classOf[EntityElem1[_, _, C] forSome { type C[_] }]).toType.decl(TermName("eItem")).asTerm

  private[this] def reflectElement(clazz: Class[_], elem: Element[_]) = {
    val instanceMirror = runtimeMirror(clazz.getClassLoader).reflect(elem)

    val fieldMirrors = ReflectionUtil.paramFieldMirrors(clazz, instanceMirror, eItemSym)

    val lmsClass = elementClassTranslations.getOrElse(clazz, elem.runtimeClass)

    ReflectedElement(lmsClass, fieldMirrors)
  }

  registerElemClass[ArrayBufferElem[_], scala.collection.mutable.ArrayBuilder[_]]
  registerElemClass[MMapElem[_, _], java.util.HashMap[_,_]]

  def elemToManifest[T](elem: Elem[T]): Manifest[_] = elem match {
    case el: ExpBaseElemEx[_,_] => {
      val tag = el.tag
      val cls = tag.mirror.runtimeClass(tag.tpe)
      Manifest.classType(cls)
    }
    case el: ExpBaseElemEx1[_,_,_] => {
      val tag = el.cont.tag
      val cls = tag.mirror.runtimeClass(tag.tpe)
      Manifest.classType(cls, elemToManifest(el.eItem))
    }
    case el: WrapperElem[_,_] =>
      elemToManifest(el.baseElem)
    case el: WrapperElem1[_,_,_,_] =>
      elemToManifest(el.baseElem)

    case el: ArrayElem[_] =>
      // see Scala bug https://issues.scala-lang.org/browse/SI-8183 (won't fix)
      val m = el.eItem match {
        case UnitElement => manifest[scala.runtime.BoxedUnit]
        case _ => elemToManifest(el.eItem)
      }
      Manifest.arrayType(m)

    case UnitElement => Manifest.Unit
    case BooleanElement => Manifest.Boolean
    case ByteElement => Manifest.Byte
    case ShortElement => Manifest.Short
    case IntElement => Manifest.Int
    case CharElement => Manifest.Char
    case LongElement => Manifest.Long
    case FloatElement => Manifest.Float
    case DoubleElement => Manifest.Double
    case StringElement => manifest[String]

    case _ =>
      val clazz = elem.getClass
      val ReflectedElement(lmsClass, fieldMirrors) = elements.getOrElseUpdate(clazz, reflectElement(clazz, elem))

      val elemParams = extractParamsByReflection(elem, fieldMirrors)
      val manifestParams = elemParams.map(e => elemToManifest(e.asInstanceOf[Elem[_]]))

      manifestParams match {
        case Nil => Manifest.classType(lmsClass)
        case h :: t => Manifest.classType(lmsClass, h, t: _*)
      }
  }
}
