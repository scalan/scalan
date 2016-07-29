/**
 * Shamelessly taken from https://github.com/namin/lms-sandbox
 */
package scalan

import java.lang.reflect.{InvocationTargetException, Method}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.{Try, Success}

import org.objenesis.ObjenesisStd

import net.sf.cglib.proxy.Enhancer
import net.sf.cglib.proxy.Factory
import net.sf.cglib.proxy.InvocationHandler
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.staged.BaseExp
import scalan.util.{StringUtil, ReflectionUtil, ScalaNameUtil}

trait Proxy { self: Scalan =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops])(implicit ct: ClassTag[Ops]): Ops
//  def proxyOpsEx[OpsBase <: AnyRef, Ops <: AnyRef, Wrapper <: Ops]
//        (x: Rep[OpsBase])
//        (implicit ctBase: ClassTag[OpsBase], ct: ClassTag[Ops], ctWrapper: ClassTag[Wrapper]): Ops

  def getStagedFunc(name: String): Rep[_] = {
    val clazz = this.getClass
    val f = clazz.getDeclaredMethod(name)
    f.invoke(this).asInstanceOf[Rep[_]]
  }

  def methodCallEx[A](receiver: Rep[_], m: Method, args: List[AnyRef])(implicit eA: Elem[A]): Rep[A]
  def newObjEx[A](args: Any*)(implicit eA: Elem[A]): Rep[A]

  /**
   * Can be thrown to prevent invoke
   */
  class DelayInvokeException extends Exception

  case class ExternalMethodException(className: String, methodName: String) extends DelayInvokeException

  def externalMethod(className: String, methodName: String) = {
    throw new ExternalMethodException(className, methodName)
  }
  def delayInvoke = throw new DelayInvokeException
}

trait ProxyStd extends Proxy { self: ScalanStd =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops])(implicit ct: ClassTag[Ops]): Ops = x
  def proxyOpsEx[OpsBase <: AnyRef, Ops <: AnyRef, Wrapper <: Ops]
                (x: Rep[OpsBase], wrapper: OpsBase => Wrapper)
                (implicit ctBase: ClassTag[OpsBase], ct: ClassTag[Ops], ctWrapper: ClassTag[Wrapper]): Ops =  {
    getProxy[OpsBase, Ops, Wrapper](x, wrapper)(ctBase, ct, ctWrapper)
  }

  private val proxies = scala.collection.mutable.Map.empty[(AnyRef, ClassTag[_]), AnyRef]
  private val objenesis = new ObjenesisStd

  private def getProxy[OpsBase <: AnyRef, Ops <: AnyRef, Wrapper <: Ops]
                      (x: OpsBase, wrapper: OpsBase => Wrapper)(ctBase: ClassTag[OpsBase], ct: ClassTag[Ops], ctWrapper: ClassTag[Wrapper]) = {
    val proxy = proxies.getOrElseUpdate((x, ct), {
      val clazz = ct.runtimeClass
      val e = new Enhancer
      e.setClassLoader(clazz.getClassLoader)
      e.setSuperclass(clazz)
      e.setCallbackType(classOf[SeqInvocationHandler[_,_,_]])
      val proxyClass = e.createClass().asSubclass(classOf[AnyRef])
      val proxyInstance = objenesis.newInstance(proxyClass).asInstanceOf[Factory]
      proxyInstance.setCallback(0, new SeqInvocationHandler[OpsBase, Ops, Wrapper](x, wrapper)(ctBase, ct, ctWrapper))
      proxyInstance
    })
    proxy.asInstanceOf[Ops]
  }

  class SeqInvocationHandler[TBase <: AnyRef, Ops <: AnyRef, Wrapper <: Ops]
                            (receiver: TBase, wrapper: TBase => Wrapper)
                            (implicit ctBase: ClassTag[TBase], ct: ClassTag[Ops], ctWrapper: ClassTag[Wrapper]) extends InvocationHandler {
    override def toString = s"SeqInvocationHandler(${receiver.toString})"

    //val clazzElem = classOf[Elem[_]]
    val wrapped = wrapper(receiver)

    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
      m.invoke(wrapped, _args: _*)
//      val clazzBase = ctBase.runtimeClass
//      val mBase = clazzBase.getMethod(m.getName, m.getParameterTypes.filterNot(c => clazzElem.isAssignableFrom(c)): _*)
//      mBase.invoke(receiver, _args: _*)
    }
  }

  def methodCallEx[A](receiver: Rep[_], m: Method, args: List[AnyRef])(implicit eA: Elem[A]): Rep[A] =
    m.invoke(receiver, args.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[A]

  def newObjEx[A](args: Any*)(implicit eA: Elem[A]): Rep[A] = {
    val types = args.map(a => a.getClass)
    val constr = eA.runtimeClass.getConstructor(types: _*)
    constr.newInstance(args.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[Rep[A]]
  }
}

trait ProxyExp extends Proxy with BaseExp with GraphVizExport { self: ScalanExp =>
  // call mkMethodCall instead of constructor
  case class MethodCall private[ProxyExp] (receiver: Exp[_], method: Method, args: List[AnyRef], neverInvoke: Boolean)(val selfType: Elem[Any]) extends Def[Any] {

    override def toString = {
      val methodStr = method.toString.replace("java.lang.", "").
        replace("public ", "").replace("abstract ", "")
      s"MethodCall($receiver, $methodStr, [${args.mkString(", ")}], $neverInvoke)"
    }

    def tryInvoke: InvokeResult =
      receiver match {
        case Def(d) =>
          val argsArray = args.toArray

          if (neverInvoke) {
            InvokeImpossible
          } else if (shouldInvoke(d, method, argsArray))
            try {
              InvokeSuccess(method.invoke(d, args: _*).asInstanceOf[Exp[_]])
            } catch {
              case e: Exception =>
                InvokeFailure(baseCause(e))
            }
          else {
            try {
              invokeSuperMethod(d, method, argsArray) match {
                case Some(res) => InvokeSuccess(res.asInstanceOf[Exp[_]])
                case None => InvokeImpossible
              }
            } catch {
              case e: Exception => InvokeFailure(baseCause(e))
            }
          }
        case _ => InvokeImpossible
      }
  }

  case class NewObject[A](eA: Elem[A], args: List[Any], neverInvoke: Boolean) extends BaseDef[A]()(eA)

  override def transformDef[A](d: Def[A], t: Transformer) = d match {
    // not the same as super because mkMethodCall can produce a more precise return type
    case MethodCall(receiver, method, args, neverInvoke) =>
      val args1 = args.map {
        case a: Exp[_] => t(a)
        case a => a
      }
      val receiver1 = t(receiver)
      // in the case neverInvoke is false, the method is invoked in rewriteDef
      mkMethodCall(receiver1, method, args1, neverInvoke).asRep[A]
    case _ => super.transformDef(d, t)
  }

  def mkMethodCall(receiver: Exp[_], method: Method, args: List[AnyRef], neverInvoke: Boolean): Exp[_] = {
    val resultElem = try {
      getResultElem(receiver, method, args)
    } catch {
      case e: Exception =>
        throwInvocationException("getResultElem for", e, receiver, method, args)
    }
    mkMethodCall(receiver, method, args, neverInvoke, resultElem)
  }

  // prefer calling the above overload
  def mkMethodCall(receiver: Exp[_], method: Method, args: List[AnyRef], neverInvoke: Boolean, resultElem: Elem[_]): Exp[_] = {
    reifyObject(MethodCall(receiver, method, args, neverInvoke)(resultElem.asElem[Any]))
  }

  @tailrec
  private def baseCause(e: Throwable): Throwable = e match {
    case e: java.lang.reflect.UndeclaredThrowableException => baseCause(e.getCause)
    case e: net.sf.cglib.proxy.UndeclaredThrowableException => baseCause(e.getCause)
    case e: InvocationTargetException => baseCause(e.getCause)
    case e: ExceptionInInitializerError => baseCause(e.getCause)
    case e => e
  }

  override protected def nodeColor(td: TypeDesc, d: Def[_])(implicit config: GraphVizConfig) = d match {
    case mc: MethodCall if mc.neverInvoke => "darkblue"
    case no: NewObject[_] if no.neverInvoke => "darkblue"
    case _ => super.nodeColor(td, d)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case MethodCall(obj, method, args, _) =>
      val methodCallStr =
        s"${ScalaNameUtil.cleanScalaName(method.getName)}(${args.mkString(", ")})"
      if (obj.isCompanion) {
        s"$obj.$methodCallStr"
      } else {
        val className = ScalaNameUtil.cleanNestedClassName(method.getDeclaringClass.getName)
        s"$obj.$className.$methodCallStr"
      }
    case NewObject(eA, args, _) =>
      val className = ScalaNameUtil.cleanNestedClassName(eA.runtimeClass.getName)
      s"new $className(${args.mkString(", ")})"
    case _ => super.formatDef(d)
  }

  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
    case call @ MethodCall(receiver, m, args, neverInvoke) =>
      call.tryInvoke match {
        // Rule: receiver.m(args) ==> body(m).subst{xs -> args}
        case InvokeSuccess(res) => res
        case InvokeFailure(e) =>
          if (e.isInstanceOf[DelayInvokeException])
            super.rewriteDef(d)
          else
            throwInvocationException("Method invocation in rewriteDef", e, receiver, m, args)
        case InvokeImpossible =>
          implicit val resultElem: Elem[T] = d.selfType
          // asRep[T] cast below should be safe
          // explicit resultElem to make sure both branches have the same type
          def copyMethodCall(newReceiver: Exp[_]) =
          mkMethodCall(newReceiver, m, args, neverInvoke, resultElem).asRep[T]

          receiver match {
            // Rule: (if(c) t else e).m(args) ==> if (c) t.m(args) else e.m(args)
            case Def(IfThenElse(cond, t, e)) =>
              IF (cond) {
                copyMethodCall(t)
              } ELSE {
                copyMethodCall(e)
              }
            case _ =>
              super.rewriteDef(d)
          }
      }

    case _ => super.rewriteDef(d)
  }

  def methodCallEx[A](receiver: Rep[_], m: Method, args: List[AnyRef])(implicit eA: Elem[A]): Rep[A] =
    mkMethodCall(receiver, m, args, true).asRep[A]

  def newObjEx[A](args: Any*)(implicit eA: Elem[A]): Rep[A] = {
    reifyObject(NewObject[A](eA, args.toList, true))
  }

  private val proxies = scala.collection.mutable.Map.empty[(Rep[_], ClassTag[_]), AnyRef]
  private val objenesis = new ObjenesisStd

  override def proxyOps[Ops <: AnyRef](x: Rep[Ops])(implicit ct: ClassTag[Ops]): Ops =
    x match {
      case Def(Const(c)) => c
      case _ => getProxy(x, ct)
    }

  private def getProxy[Ops](x: Rep[Ops], ct: ClassTag[Ops]) = {
    val proxy = proxies.getOrElseUpdate((x, ct), {
      val clazz = ct.runtimeClass
      val e = new Enhancer
      e.setClassLoader(clazz.getClassLoader)
      e.setSuperclass(clazz)
      e.setCallbackType(classOf[ExpInvocationHandler[_]])
      val proxyClass = e.createClass().asSubclass(classOf[AnyRef])
      val proxyInstance = objenesis.newInstance(proxyClass).asInstanceOf[Factory]
      proxyInstance.setCallback(0, new ExpInvocationHandler(x))
      proxyInstance
    })
    proxy.asInstanceOf[Ops]
  }

  final val skipInterfaces = {
    val mirror = scala.reflect.runtime.currentMirror
    Symbols.SuperTypesOfDef.flatMap { sym =>
      Try[Class[_]](mirror.runtimeClass(sym.asClass)).toOption
    }
  }

  private def getSuperMethod(m: Method): Option[Method] = {
    val c = m.getDeclaringClass
    val superClass = c.getSuperclass
    val optClassMethod =
      if (superClass == null || skipInterfaces.contains(superClass))
        None
      else
        Try(superClass.getMethod(m.getName, m.getParameterTypes: _*)).toOption
    optClassMethod.orElse {
      val is = c.getInterfaces
      val methods = is.toIterator
        .filterNot(i => skipInterfaces.contains(i))
        .map(i => Try(i.getMethod(m.getName, m.getParameterTypes: _*)))
      val optInterfaceMethod = methods.collectFirst { case Success(m) => m }
      optInterfaceMethod
    }
  }

  // FIXME this is a hack, this should be handled in Passes
  // The problem is that rewriting in ProgramGraph.transform is non-recursive
  // We need some way to make isInvokeEnabled local to graph
  type InvokeTester = (Def[_], Method) => Boolean

  // we need to always invoke these for creating default values
  case class NamedUnpackTester(name: String, tester: UnpackTester) extends UnpackTester {
    def apply(e: Elem[_]) = tester(e)
  }

  case class NamedInvokeTester(name: String, tester: InvokeTester) extends InvokeTester {
    def apply(d: Def[_], m: Method) = tester(d, m)
  }

  private val isCompanionApply: InvokeTester = NamedInvokeTester("isCompanionApply",
    (_, m) => m.getName == "apply" && m.getDeclaringClass.getName.endsWith("CompanionAbs")
  )

  private val isFieldGetterCache = collection.mutable.Map.empty[(Type, Method), Boolean]

  private val isFieldGetter: InvokeTester = NamedInvokeTester("isFieldGetter", { (d, m) =>
    val tpe = d.selfType.tag.tpe
    isFieldGetterCache.getOrElseUpdate((tpe, m),
      findScalaMethod(tpe, m).isParamAccessor)
  })

  protected val initialInvokeTesters = Set(isCompanionApply, isFieldGetter)
  private var invokeTesters: Set[InvokeTester] = initialInvokeTesters

  protected def invokeAll = false

  def isInvokeEnabled(d: Def[_], m: Method) = invokeAll || {
    if (_currentPass != null) {
      _currentPass.isInvokeEnabled(d, m).getOrElse {
        invokeTesters.exists(_(d, m))
      }
    }
    else
      invokeTesters.exists(_(d, m))
  }

  protected def shouldInvoke(d: Def[_], m: Method, args: Array[AnyRef]) = {
    if (m.getDeclaringClass.isAssignableFrom(d.getClass)) {
      isInvokeEnabled(d, m) || hasFuncArg(args) ||
        // e.g. for methods returning Elem
        (m.getReturnType != classOf[AnyRef] && m.getReturnType != classOf[Exp[_]])
    } else {
      //val parent = commonAncestor(m.getDeclaringClass, d.getClass)
      false
    }
  }

  def addInvokeTester(pred: InvokeTester): Unit = {
    invokeTesters += pred
  }

  def removeInvokeTester(pred: InvokeTester): Unit = {
    invokeTesters -= pred
  }

  def resetTesters() = {
    invokeTesters = initialInvokeTesters
    unpackTesters = initialUnpackTesters
  }

  protected def hasFuncArg(args: Array[AnyRef]): Boolean =
    args.exists {
      case f: Function0[_] => true
      case f: Function1[_, _] => true
      case f: Function2[_, _, _] => true
      case _ => false
    }

  private def invokeSuperMethod(d: Def[_], m: Method, args: Array[AnyRef]): Option[AnyRef] = {
    @tailrec
    def loop(m: Method): Option[AnyRef] =
      getSuperMethod(m) match {
        case None => None
        case Some(superMethod) =>
          if (shouldInvoke(d, superMethod, args))
            Some(superMethod.invoke(d, args: _*))
          else
            loop(superMethod)
      }

    loop(m)
  }

  // stack of receivers for which MethodCall nodes should be created by InvocationHandler
  protected var methodCallReceivers = Set.empty[Exp[_]]

  import Symbols._

  def elemFromType(tpe: Type, elemMap: Map[Symbol, TypeDesc], baseType: Type): Elem[_] = tpe.dealias match {
    case TypeRef(_, classSymbol, params) => classSymbol match {
      case UnitSym => UnitElement
      case BooleanSym => BooleanElement
      case ByteSym => ByteElement
      case ShortSym => ShortElement
      case IntSym => IntElement
      case LongSym => LongElement
      case FloatSym => FloatElement
      case DoubleSym => DoubleElement
      case StringSym => StringElement
      case PredefStringSym => StringElement
      case CharSym => CharElement
      case Tuple2Sym =>
        val eA = elemFromType(params(0), elemMap, baseType)
        val eB = elemFromType(params(1), elemMap, baseType)
        pairElement(eA, eB)
      case EitherSym =>
        val eA = elemFromType(params(0), elemMap, baseType)
        val eB = elemFromType(params(1), elemMap, baseType)
        sumElement(eA, eB)
      case Function1Sym =>
        val eA = elemFromType(params(0), elemMap, baseType)
        val eB = elemFromType(params(1), elemMap, baseType)
        funcElement(eA, eB)
      case ArraySym =>
        val eItem = elemFromType(params(0), elemMap, baseType)
        arrayElement(eItem)
      case ListSym =>
        val eItem = elemFromType(params(0), elemMap, baseType)
        listElement(eItem)
      case _ if classSymbol.asType.isParameter =>
        getDesc(elemMap, classSymbol, s"Can't create element for abstract type $tpe") match {
          case elem: Elem[_] => elem
          case cont: Cont[_] =>
            val paramElem = elemFromType(params(0), elemMap, baseType)
            cont.lift(paramElem)
        }
      case _ if classSymbol.isClass =>
        val paramDescs = params.zip(classSymbol.asType.toTypeConstructor.typeParams).map {
          case (typaram, formalParam) if typaram.takesTypeArgs =>
            // handle high-kind argument
            typaram match {
              case TypeRef(_, classSymbol, _) =>
                val desc = getDesc(elemMap, classSymbol, s"Can't find the descriptor for type argument $typaram of $tpe")
                desc match {
                  case cont: Cont[_] => cont
                  case _ =>
                    !!!(s"Expected a Cont, got $desc for type argument $typaram of $tpe")
                }
              case PolyType(_, _) =>
                // fake to make the compiler happy
                type F[A] = A

                new Cont[F] {
                  private val elemMap1: Map[Symbol, TypeDesc] = elemMap + (formalParam -> this)

                  def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[F[T]] = ???
                  def lift[T](implicit eT: Elem[T]): Elem[F[T]] = {
                    val tpe1 = appliedType(typaram, List(eT.tag.tpe))
                    // TODO incorrect baseType
                    elemFromType(tpe1, elemMap1, baseType).asInstanceOf[Elem[F[T]]]
                  }
                  def unlift[T](implicit eFT: Elem[F[T]]) = ???
                  def getElem[T](fa: Exp[F[T]]) = ???
                  def unapply[T](e: Elem[_]) = ???
                  override def getName(f: TypeDesc => String) = typaram.toString
                }
            }
          case (typaram, _) =>
            elemFromType(typaram, elemMap, baseType)
        }

        val descClasses = paramDescs.map {
          case e: Elem[_] =>
            classOf[Elem[_]]
          case c: Cont[_] =>
            // works due to type erasure; should be classOf[Cont[_]], but that's invalid syntax
            // See other uses of Cont[Any] here as well
            classOf[Cont[Any]]
          case d => !!!(s"Unknown type descriptior $d")
        }.toArray
        // entity type or base type
        if (classSymbol.asClass.isTrait || classSymbol == baseType.typeSymbol) {
          // abstract case, call *Element
          val methodName = StringUtil.lowerCaseFirst(classSymbol.name.toString) + "Element"
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
        } else {
          // concrete case, call viewElement(*Iso)
          val methodName = "iso" + classSymbol.name.toString
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
    case _ => !!!(s"Failed to create element from type $tpe")
  }

  private def getDesc(elemMap: Map[Symbol, TypeDesc], sym: Symbol, errorMessage: => String): TypeDesc =
    elemMap.get(sym).orElse {
      // For higher-kinded type parameters we may end with unequal symbols here
      elemMap.collectFirst {
        case (sym1, d) if sym.name == sym1.name && (internal.isFreeType(sym) || internal.isFreeType(sym1)) => d
      }
    }.getOrElse(!!!(errorMessage))

  private def extractParts(elem: Elem[_], classSymbol: Symbol, params: List[Type], tpe: Type): List[(TypeDesc, Type)] = classSymbol match {
    case UnitSym | BooleanSym | ByteSym | ShortSym | IntSym | LongSym |
         FloatSym | DoubleSym | StringSym | PredefStringSym | CharSym =>
      Nil
    case Tuple2Sym =>
      val elem1 = elem.asInstanceOf[PairElem[_, _]]
      List(elem1.eFst -> params(0), elem1.eSnd -> params(1))
    case EitherSym =>
      val elem1 = elem.asInstanceOf[SumElem[_, _]]
      List(elem1.eLeft -> params(0), elem1.eRight -> params(1))
    case Function1Sym =>
      val elem1 = elem.asInstanceOf[FuncElem[_, _]]
      List(elem1.eDom -> params(0), elem1.eRange -> params(1))
    case ArraySym =>
      val elem1 = elem.asInstanceOf[ArrayElem[_]]
      List(elem1.eItem -> params(0))
    case ListSym =>
      val elem1 = elem.asInstanceOf[ListElem[_]]
      List(elem1.eItem -> params(0))
    case _ if classSymbol.isClass =>
      val declarations = classSymbol.asClass.selfType.decls
      val res = declarations.flatMap {
        case member if member.isMethod =>
          val memberTpe = member.asMethod.returnType.dealias
          memberTpe match {
            // member returning Elem, such as eItem
            case TypeRef(_, ElementSym, params) =>
              val param = params(0).asSeenFrom(tpe, classSymbol)
              // There should be a method with the same name on the corresponding element class
              val elem1 = getParameterTypeDesc(elem, member.name.toString).asInstanceOf[Elem[_]]
              List(elem1 -> param)
            case TypeRef(_, ContSym, params) =>
              val param = params(0).asSeenFrom(tpe, classSymbol)
              // There should be a method with the same name on the corresponding element class
              val cont1 = getParameterTypeDesc(elem, member.name.toString).asInstanceOf[Cont[Any]]
              List(cont1 -> param)
            case _ => Nil
          }
      }.toList
      res
  }

  protected def getParameterTypeDesc(elem: Elem[_], paramName: String): AnyRef = {
    val correspondingElemMethod = elem.getClass.getMethod(paramName)
    val desc = correspondingElemMethod.invoke(elem)
    desc
  }

  @tailrec
  private def extractElems(elemsWithTypes: List[(TypeDesc, Type)],
                           unknownParams: Set[Symbol],
                           knownParams: Map[Symbol, TypeDesc]): Map[Symbol, TypeDesc] =
    if (unknownParams.isEmpty)
      knownParams
    else
      elemsWithTypes match {
        case Nil =>
          knownParams
        case (elem: Elem[_], tpe) :: rest =>
          tpe.dealias match {
            case TypeRef(_, classSymbol, params) => classSymbol match {
              case _ if classSymbol.asType.isParameter =>
                extractElems(rest, unknownParams - classSymbol, knownParams.updated(classSymbol, elem))
              case _ =>
                val elemParts = extractParts(elem, classSymbol, params, tpe)
                extractElems(elemParts ++ rest, unknownParams, knownParams)
            }
            case _ =>
              !!!(s"$tpe was not a TypeRef")
          }
        case (cont: Cont[_], tpe) :: rest =>
          val classSymbol = tpe.dealias match {
            case TypeRef(_, classSymbol, _) => classSymbol
            case PolyType(_, TypeRef(_, classSymbol, _)) => classSymbol
            case _ => !!!(s"Failed to extract symbol from $tpe")
          }

          if (classSymbol.asType.isParameter)
            extractElems(rest, unknownParams - classSymbol, knownParams.updated(classSymbol, cont))
          else {
//            val elem = cont.lift(UnitElement)
//            val elemParts = extractParts(elem, classSymbol, List(typeOf[Unit]), tpe)
            extractElems(/*elemParts ++ */rest, unknownParams, knownParams)
          }
      }

  // TODO Combine with extractElems
  private def getElemsMapFromInstanceElem(e: Elem[_], tpe: Type): Map[Symbol, TypeDesc] = {
    e match {
      case _: EntityElem[_] =>
        val kvs = tpe.dealias.typeSymbol.asType.typeParams.map {
          case sym =>
            val res = Try {
              if (sym.asType.toTypeConstructor.takesTypeArgs) {
                // FIXME hardcoding - naming convention is assumed to be consistent with ScalanCodegen
                val methodName = "c" + sym.name.toString
                val cont = invokeMethod(e, methodName).asInstanceOf[Cont[Any]]
                (cont, Map.empty[Symbol, TypeDesc])
              } else {
                val methodName = "e" + sym.name.toString
                val elem = invokeMethod(e, methodName).asInstanceOf[Elem[_]]
                val map1 = getElemsMapFromInstanceElem(elem, tpeFromElem(elem))
                (elem, map1)
              }
            }
            (sym, res)
        }
        val successes = kvs.collect { case (k, Success((desc, map))) => (k, desc, map) }
        val maps = successes.map(_._3)
        val map0 = successes.map { case (k, desc, _) => (k, desc) }.toMap
        maps.fold(map0)(_ ++ _)
      // The above lookup isn't necessary for other elements
      case _ => Map.empty
    }
  }

  private def invokeMethod(obj: AnyRef, methodName: String): AnyRef = {
    try {
      val method = obj.getClass.getMethod(methodName)
      try {
        val result = method.invoke(obj)
        result
      } catch {
        case e: Exception =>
          !!!(s"Failed to invoke $methodName of object $obj", e)
      }
    } catch {
      case _: NoSuchMethodException =>
        !!!(s"Failed to find method with name $methodName of object $obj")
    }
  }

  private def findScalaMethod(tpe: Type, m: Method) = {
    val scalaMethod0 = tpe.member(TermName(m.getName))
    if (scalaMethod0.isTerm) {
      val overloads = scalaMethod0.asTerm.alternatives
      val scalaMethod1 = (if (overloads.length == 1) {
        scalaMethod0
      } else {
        val javaOverloadId = ReflectionUtil.overloadId(m)

        overloads.find { sym =>
          !isSupertypeOfDef(sym.owner) && {
            val scalaOverloadId = ReflectionUtil.annotation[OverloadId](sym).map { sAnnotation =>
              val annotationArgs = sAnnotation.tree.children.tail
              val AssignOrNamedArg(_, Literal(Constant(sOverloadId))) = annotationArgs.head
              sOverloadId
            }
            scalaOverloadId == javaOverloadId
          }
        }.getOrElse {
          val overloadIdString = javaOverloadId.fold("no overload id")(x => s"""overload id "$x"""")
          !!!(s"Method with name ${m.getName} and $overloadIdString doesn't exist on type $tpe")
        }
      }).asMethod

      if (scalaMethod1.returnType.typeSymbol != definitions.NothingClass) {
        scalaMethod1
      } else {
        scalaMethod1.overrides.find(_.asMethod.returnType.typeSymbol != definitions.NothingClass).getOrElse {
          !!!(s"Method $scalaMethod1 on type $tpe and all overridden methods return Nothing")
        }.asMethod
      }
    } else
      !!!(s"Method $m couldn't be found on type $tpe")
  }

  protected def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]): Elem[_] = {
    val e = receiver.elem
    val tpe = tpeFromElem(e)
    val instanceElemMap = getElemsMapFromInstanceElem(e, tpe)
    val scalaMethod = findScalaMethod(tpe, m)

    // http://stackoverflow.com/questions/29256896/get-precise-return-type-from-a-typetag-and-a-method
    val returnType = scalaMethod.returnType.asSeenFrom(tpe, scalaMethod.owner).dealias
    returnType match {
      // FIXME can't figure out proper comparison with RepType here
      case TypeRef(_, sym, List(tpe1)) if sym.name.toString == "Rep" =>
        val paramTypes = scalaMethod.paramLists.flatten.map(_.typeSignature.asSeenFrom(tpe, scalaMethod.owner).dealias)
        // reverse to let implicit elem parameters be first
        val elemsWithTypes: List[(TypeDesc, Type)] = args.zip(paramTypes).reverse.flatMap {
          case (e: Exp[_], TypeRef(_, sym, List(tpeE))) if sym.name.toString == "Rep" =>
            List(e.elem -> tpeE)
          case (elem: Elem[_], TypeRef(_, ElementSym, List(tpeElem))) =>
            List(elem -> tpeElem)
          case (cont: Cont[_], TypeRef(_, ContSym, List(tpeCont))) =>
            List(cont -> tpeCont)
          // below cases can be safely skipped without doing reflection
          case (_: Function0[_] | _: Function1[_, _] | _: Function2[_, _, _] | _: Numeric[_] | _: Ordering[_], _) => Nil
          case (obj, tpeObj) =>
            tpeObj.members.flatMap {
              case method: MethodSymbol if method.paramLists.isEmpty =>
                method.returnType.dealias match {
                  case TypeRef(_, ElementSym | ContSym, List(tpeElemOrCont)) =>
                    // TODO this doesn't work in InteractAuthExamplesTests.crossDomainStaged
                    // due to an apparent bug in scala-reflect. Test again after updating to 2.11

                    // val objMirror = runtimeMirror.reflect(obj)
                    // val methodMirror = objMirror.reflectMethod(method)
                    val jMethod = ReflectionUtil.methodToJava(method)
                    jMethod.invoke(obj) /* methodMirror.apply() */ match {
                      case elem: Elem[_] =>
                        List(elem -> tpeElemOrCont.asSeenFrom(tpeObj, method.owner))
                      case cont: Cont[_] =>
                        List(cont -> tpeElemOrCont.asSeenFrom(tpeObj, method.owner))
                      case x =>
                        !!!(s"$tpeObj.$method must return Elem or Cont but returned $x")
                    }
                  case _ =>
                    Nil
                }
              case _ => Nil
            }
        }

        try {
          val paramElemMap = extractElems(elemsWithTypes, scalaMethod.typeParams.toSet, Map.empty)
          val elemMap = instanceElemMap ++ paramElemMap
          // check if return type is a TypeWrapper
          val baseType = tpe.baseType(TypeWrapperSym) match {
            case NoType => definitions.NothingTpe
            // e.g. Throwable from TypeWrapper[Throwable, SThrowable]
            case TypeRef(_, _, params) => params(0).asSeenFrom(tpe, scalaMethod.owner)
            case unexpected => !!!(s"unexpected result from ${tpe1}.baseType(BaseTypeEx): $unexpected")
          }

          elemFromType(tpe1, elemMap, baseType)
        } catch {
          case e: Exception =>
            !!!(s"Failure to get result elem for method $m on type $tpe\nReturn type: $returnType", e)
        }
      case _ =>
        !!!(s"Return type of method $m should be a Rep, but is $returnType")
    }
  }

  private def tpeFromElem(e: Elem[_]): Type = {
    val tag = e match {
      case baseTypeElem: BaseTypeElem[_, _] => baseTypeElem.wrapperElem.tag
      case _ => e.tag
    }
    tag.tpe
  }

  private object Symbols {
    val RepSym = typeOf[Rep[_]].typeSymbol

    val UnitSym = typeOf[Unit].typeSymbol
    val BooleanSym = typeOf[Boolean].typeSymbol
    val ByteSym = typeOf[Byte].typeSymbol
    val ShortSym = typeOf[Short].typeSymbol
    val IntSym = typeOf[Int].typeSymbol
    val LongSym = typeOf[Long].typeSymbol
    val FloatSym = typeOf[Float].typeSymbol
    val DoubleSym = typeOf[Double].typeSymbol
    val StringSym = typeOf[String].typeSymbol
    val PredefStringSym = definitions.PredefModule.moduleClass.asType.toType.member(TypeName("String"))
    val CharSym = typeOf[Char].typeSymbol

    val Tuple2Sym = typeOf[(_, _)].typeSymbol
    val EitherSym = typeOf[_ | _].typeSymbol
    val Function1Sym = typeOf[_ => _].typeSymbol
    val ArraySym = typeOf[Array[_]].typeSymbol
    val ListSym = typeOf[List[_]].typeSymbol

    val TypeWrapperSym = typeOf[TypeWrapper[_, _]].typeSymbol

    val ElementSym = typeOf[Elem[_]].typeSymbol
    val ContSym = typeOf[Cont[Any]].typeSymbol

    val SuperTypesOfDef = typeOf[Def[_]].baseClasses.toSet
  }

  private def isSupertypeOfDef(clazz: Symbol) =
    Symbols.SuperTypesOfDef.contains(clazz)

  val externalClassNameMetaKey = MetaKey[String]("externalClassName")
  val externalMethodNameMetaKey = MetaKey[String]("externalMethodName")

  sealed trait InvokeResult

  case class InvokeSuccess(result: Rep[_]) extends InvokeResult
  case class InvokeFailure(exception: Throwable) extends InvokeResult
  case object InvokeImpossible extends InvokeResult

  class ExpInvocationHandler[T](receiver: Exp[T]) extends InvocationHandler {
    override def toString = s"ExpInvocationHandler(${receiver.toStringWithDefinition})"

    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
      val args = if (_args == null) Array.empty[AnyRef] else _args

      def mkMethodCall(neverInvoke: Boolean) =
        ProxyExp.this.mkMethodCall(receiver, m, args.toList, neverInvoke)

      receiver match {
        case Def(d) =>
          if (shouldInvoke(d, m, args)) {
            val res = try {
              // call method of the node when it's allowed
              m.invoke(d, args: _*)
            } catch {
              case e: Exception =>
                baseCause(e) match {
                  case ExternalMethodException(className, methodName) =>
                    mkMethodCall(neverInvoke = true)
                      .setMetadata(externalClassNameMetaKey)(className)
                      .setMetadata(externalMethodNameMetaKey)(methodName)
                  case cause =>
                    throwInvocationException("Method invocation", cause, receiver, m, args)
                }
            }
            res
          } else {
            // try to call method m via inherited class or interfaces
            invokeSuperMethod(d, m, args).getOrElse {
              mkMethodCall(neverInvoke = false)
            }
          }
        case _ => mkMethodCall(neverInvoke = false)
      }
    }
  }

  def throwInvocationException(whatFailed: String, cause: Throwable, receiver: Exp[_], m: Method, args: Seq[Any]) = {
    val deps = receiver +: args.flatMap(syms)
    !!!(s"$whatFailed $receiver.${m.getName}(${args.mkString(", ")}) failed", baseCause(cause), deps: _*)
  }
}
