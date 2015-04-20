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
  def newObjEx[A](c: Class[A], args: List[Rep[Any]])(implicit eA: Elem[A]): Rep[A]

  def patternMatchError(obj: Any): Nothing
}

trait ProxySeq extends Proxy { self: ScalanSeq =>
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

  def newObjEx[A](c: Class[A], args: List[Rep[Any]])(implicit eA: Elem[A]): Rep[A] = {
    val types = args.map(a => a.getClass)
    val constr = c.getConstructor(types: _*)
    constr.newInstance(args.map(_.asInstanceOf[AnyRef]): _*) //.asInstanceOf[Rep[A]]
  }

  def patternMatchError(obj: Any) = throw new MatchError(obj)
}

trait ProxyExp extends Proxy with BaseExp with GraphVizExport { self: ScalanExp =>
  // call mkMethodCall instead of constructor
  case class MethodCall private[ProxyExp] (receiver: Exp[_], method: Method, args: List[AnyRef], neverInvoke: Boolean)(val selfType: Elem[Any]) extends Def[Any] {
    def uniqueOpId = s"$name:${method.getName}"
    override def mirror(t: Transformer) = {
      val args1 = args.map {
        case a: Exp[_] => t(a)
        case a => a
      }
      val receiver1 = t(receiver)
      mkMethodCall(receiver1, method, args1, neverInvoke)
    }

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

  case class NewObject[T](clazz: Class[T] , args: List[AnyRef], neverInvoke: Boolean)(implicit selfType: Elem[T]) extends BaseDef[T] {
    def uniqueOpId = s"new $name"
    override def mirror(t: Transformer) = {
      val args1 = args.map {
        case a: Exp[_] => t(a)
        case a => a
      }
      NewObject[T](clazz, args1, neverInvoke)
    }
  }

  def mkMethodCall(receiver: Exp[_], method: Method, args: List[AnyRef], neverInvoke: Boolean): Exp[_] = {
    val resultElem = getResultElem(receiver, method, args)
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

  override protected def nodeColor(sym: Exp[_])(implicit config: GraphVizConfig) = sym match {
    case Def(d) => d match {
      case mc: MethodCall if mc.neverInvoke => "darkblue"
      case no: NewObject[_] if no.neverInvoke => "darkblue"
      case _ => super.nodeColor(sym)
    }
    case _ => super.nodeColor(sym)
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
    case NewObject(c, args, _) =>
      val className = ScalaNameUtil.cleanNestedClassName(c.getName)
      s"new $className(${args.mkString(", ")})"
    case _ => super.formatDef(d)
  }

  def methodCallEx[A](receiver: Rep[_], m: Method, args: List[AnyRef])(implicit eA: Elem[A]): Rep[A] =
    mkMethodCall(receiver, m, args, true).asRep[A]

  def newObjEx[A](c: Class[A], args: List[Rep[Any]])(implicit eA: Elem[A]): Rep[A] = {
    new NewObject[A](c, args, true)
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
  private val companionApply: InvokeTester =
    (_, m) => m.getName == "apply" && m.getDeclaringClass.getName.endsWith("CompanionAbs")
  private var invokeTesters: Set[InvokeTester] = Set(companionApply)

  def isInvokeEnabled(d: Def[_], m: Method) = invokeTesters.exists(_(d, m))

  private def shouldInvoke(d: Def[_], m: Method, args: Array[AnyRef]) = {
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

  type SomeCont = Cont[C] forSome { type C[_] }
  type TypeDesc = Elem[_] | SomeCont

  private def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]): Elem[_] = {
    val e = receiver.elem
    val tag = e match {
      case extE: BaseElemEx[_,_] => extE.getWrapperElem.tag
      case _ => e.tag
    }
    val tpe = tag.tpe
    val instanceElemMap = getElemsMapFromInstanceElem(e, tpe)
    val scalaMethod = findScalaMethod(tpe, m)
    // http://stackoverflow.com/questions/29256896/get-precise-return-type-from-a-typetag-and-a-method
    val returnType = scalaMethod.returnType.asSeenFrom(tpe, scalaMethod.owner).normalize
    returnType match {
      // FIXME can't figure out proper comparison with RepType here
      case TypeRef(_, sym, List(tpe1)) if sym.name.toString == "Rep" =>
        // FIXME assumed for now to correspond to the method's type params, in the same order
        val elemParams = args.collect {
          case e: Elem[_] => scala.Left(e)
          case c: SomeCont @unchecked => scala.Right[Elem[_], SomeCont](c)
        }
        val elemMap = instanceElemMap ++ scalaMethod.typeParams.zip(elemParams).toMap
        // check if return type is a TypeWrapper
        val baseType = tpe.baseType(Symbols.TypeWrapperSym) match {
          case NoType => definitions.NothingTpe
          // e.g. Throwable from TypeWrapper[Throwable, SThrowable]
          case TypeRef(_, _, params) => params(0).asSeenFrom(tpe, scalaMethod.owner)
          case unexpected => !!!(s"unexpected result from ${tpe1}.baseType(TypeWrapper): $unexpected")
        }
        elemFromType(tpe1, elemMap, baseType)
      case _ =>
        !!!(s"Return type of method $m should be a Rep, but is $returnType")
    }
  }

  private def getElemsMapFromInstanceElem(e: Elem[_], tpe: Type): Map[Symbol, TypeDesc] = {
    val kvs = tpe.typeSymbol.asType.typeParams.collect {
      case sym if sym.isParameter =>
        if (sym.asType.typeParams.size > 0) {
          // FIXME hardcoding - naming convention is assumed to be consistent with ScalanCodegen
          val methodName = "c" + sym.name.toString
          val value = try {
            val res = invokeMethod(e, methodName).asInstanceOf[SomeCont]
            scala.Right[Elem[_], SomeCont](res)
          } catch {
            case _: Throwable => null
          }
          (sym -> value)
        }
        else {
          val methodName = "e" + sym.name.toString
          val value = try {
            val res = invokeMethod(e, methodName).asInstanceOf[Elem[_]]
            scala.Left[Elem[_], SomeCont](res)
          } catch {
            case _: Throwable => null
          }
          (sym -> value)
        }
    }
    kvs.filter { case (k,v) => v != null }.toMap
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
        !!!(s"Failed to find method with name $methodName  of object $obj")
    }
  }

  private def findScalaMethod(tpe: Type, m: Method) = {
    val scalaMethod0 = tpe.member(newTermName(m.getName))
    if (scalaMethod0.isTerm) {
      val overloads = scalaMethod0.asTerm.alternatives
      (if (overloads.length == 1) {
        scalaMethod0
      } else {
        val javaOverloadId = m.getAnnotation(classOf[OverloadId]) match {
          case null => None
          case jAnnotation => Some(jAnnotation.value)
        }

        overloads.find { sym =>
          !isSupertypeOfDef(sym.owner) && {
            val scalaOverloadId = ReflectionUtil.annotation[OverloadId](sym).map { sAnnotation =>
              val LiteralArgument(Constant(sOverloadId)) = sAnnotation.javaArgs.head._2
              sOverloadId
            }
            scalaOverloadId == javaOverloadId
          }
        }.get
      }).asMethod
    } else
      !!!(s"Method $m couldn't be found on type $tpe")
  }

  import Symbols._
  private def elemFromType(tpe: Type, elemMap: Map[Symbol, TypeDesc], baseType: Type): Elem[_] = tpe.normalize match {
    case TypeRef(_, classSymbol, params) => classSymbol match {
      case UnitSym => UnitElement
      case BooleanSym => BoolElement
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
        pairElement(elemFromType(params(0), elemMap, baseType), elemFromType(params(1), elemMap, baseType))
      case EitherSym =>
        sumElement(elemFromType(params(0), elemMap, baseType), elemFromType(params(1), elemMap, baseType))
      case Function1Sym =>
        funcElement(elemFromType(params(0), elemMap, baseType), elemFromType(params(1), elemMap, baseType))
      case ArraySym =>
        arrayElement(elemFromType(params(0), elemMap, baseType))
      case ListSym =>
        listElement(elemFromType(params(0), elemMap, baseType))
      case _ if classSymbol.asType.isAbstractType =>
        elemMap.getOrElse(classSymbol, !!!(s"Can't create element for abstract type $tpe")).left.get
      case _ if classSymbol.isClass =>
        //val elemClasses = Array.fill[Class[_]](params.length)(classOf[Element[_]])
        val paramDescs = params.map(typaram => {
          typaram match {
            case TypeRef(_, classSymbol, params) if typaram.takesTypeArgs =>
              // handle high-kind argument
              // FIXME use better way of lookup other than by names (to avoid name collisions)
              val optRes = elemMap.find {case (sym, d) => sym.name == classSymbol.name}
              val res = optRes.getOrElse(!!!(s"Can't find descriptor for type argument $typaram of $tpe"))._2.right.get
              res
            case _ =>
              elemFromType(typaram, elemMap, baseType)
          }
        })

        val descClasses = paramDescs.map {
          case e: Elem[_] => classOf[Elem[_]]
          case c: SomeCont @unchecked => classOf[SomeCont]
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
              !!!(s"Failed to find element-creating method with name $methodName and ${params.length} Element parameters")
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
            case _: NoSuchMethodException =>
              !!!(s"Failed to find iso-creating method with name $methodName and ${params.length} Element parameters")
          }
        }
    }
//    case PolyType(params, resultType) =>
//      tpe match {
//        case TypeRef(_, classSymbol, params) =>
//          val res = elemMap.getOrElse(classSymbol, !!!(s"Can't create element for abstract type $tpe")).right.get
//          res
//      }
    case _ => ???(s"Failed to create element from type $tpe")
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
    val PredefStringSym = definitions.PredefModule.moduleClass.asType.toType.member(newTypeName("String"))
    val CharSym = typeOf[Char].typeSymbol

    val Tuple2Sym = typeOf[(_, _)].typeSymbol
    val EitherSym = typeOf[_ | _].typeSymbol
    val Function1Sym = typeOf[_ => _].typeSymbol
    val ArraySym = typeOf[Array[_]].typeSymbol
    val ListSym = typeOf[List[_]].typeSymbol

    val TypeWrapperSym = typeOf[TypeWrapper[_, _]].typeSymbol

    val SuperTypesOfDef = typeOf[Def[_]].baseClasses.toSet
  }

  private def isSupertypeOfDef(clazz: Symbol) =
    SuperTypesOfDef.contains(clazz)

  sealed trait InvokeResult

  case class InvokeSuccess(result: Rep[_]) extends InvokeResult
  case class InvokeFailure(exception: Throwable) extends InvokeResult
  case object InvokeImpossible extends InvokeResult

  class ExpInvocationHandler[T](receiver: Exp[T]) extends InvocationHandler {
    override def toString = s"ExpInvocationHandler(${receiver.toStringWithDefinition})"

    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
      val args = if (_args == null) Array.empty[AnyRef] else _args
      receiver match {
        case Def(d) =>
          if (shouldInvoke(d, m, args)) {
            try {
              // call method of the node when it's allowed
              val res = m.invoke(d, args: _*)
              res
            } catch {
              case e: Exception => !!!("Method invocation failed", baseCause(e))
            }
          } else {
            // try to call method m via inherited class or interfaces
            val optRes = invokeSuperMethod(d, m, args)
            optRes match {
              case Some(res) => res
              case None =>
                invokeMethodOfVar(m, args)
            }
          }
        case _ => invokeMethodOfVar(m, args)
      }
    }

    def invokeMethodOfVar(m: Method, args: Array[AnyRef]) = {
      mkMethodCall(receiver, m, args.toList, false)
      //      /* If invoke is enabled or current method has arg of type <function> - do not create methodCall */
      //      if (methodCallReceivers.contains(receiver) || !shouldInvoke(args)) {
      //        createMethodCall(m, args)
      //      } else {
      //        receiver.elem match {
      //          case e: ViewElem[_, _] =>
      //            val iso = e.iso
      //            methodCallReceivers += receiver
      //            // adds receiver to the program graph
      //            val wrapper = iso.to(iso.from(receiver))
      //            methodCallReceivers -= receiver
      //            wrapper match {
      //              case Def(d) if canInvoke(m, d) =>
      //                val res = m.invoke(d, args: _*)
      //                res
      //              case _ =>
      //                (new ExpInvocationHandler(wrapper, forceInvoke)).createMethodCall(m, args)
      //            }
      //          case e => !!!(s"Receiver ${receiver.toStringWithType} must be a user type, but its elem is ${e}")
      //        }
      //      }
    }

    // code from Scalan
//    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
//      val args = if (_args == null) Array.empty[AnyRef] else _args
//      receiver match {
//        case Def(d) if (canInvoke(m, d) && (isInvokeEnabled(m) || hasFuncArg(args))) => {
//          // call method of the node
//          val res = m.invoke(d, args: _*)
//          res
//        }
//        case _ =>
//          invokeMethodOfVar(m, args)
//      }
//    }
//
//    def invokeMethodOfVar(m: Method, args: Array[AnyRef]) = {
//      /* If invoke is enabled or current method has arg of type <function> - do not create methodCall */
//      if (methodCallReceivers.contains(receiver) || !(isInvokeEnabled(m) || hasFuncArg(args))) {
//        createMethodCall(m, args)
//      } else {
//        getIso match {
//          case iso =>
//            methodCallReceivers += receiver
//            // adds receiver to the program graph
//            val wrapper = iso.to(iso.from(receiver))
//            methodCallReceivers -= receiver
//            wrapper match {
//              case Def(d) if canInvoke(m, d) =>
//                val res = m.invoke(d, args: _*)
//                res
//              case _ =>
//                (new ExpInvocationHandler(wrapper)).createMethodCall(m, args)
//            }
//        }
//      }
//    }
//
//    def getIso: Iso[_, T] = receiver.elem match {
//      case e: ViewElem[_, T] @unchecked => e.iso
//      case _ =>
//        !!!(s"Receiver ${receiver.toStringWithType} should have ViewElem, but has ${receiver.elem} instead")
//    }
//
//    // TODO cache result elements
//    def getResultElem(m: Method, args: Array[AnyRef]): Elem[_] = {
//      val iso = getIso
//      val zero = iso.defaultRepTo.value
//      val Def(zeroNode) = zero
//      try {
//        val res = m.invoke(zeroNode, args: _*)
//        res.asInstanceOf[Exp[_]].elem
//      } catch {
//        case e: InvocationTargetException =>
//          e.getCause match {
//            case e1: ElemException[_] => e1.element
//            case _ => throw e
//          }
//      }
//    }

  }

  def patternMatchError(obj: Any): Nothing = throw new DelayInvokeException

  /**
   * Can be thrown to prevent invoke
   */
  class DelayInvokeException extends Exception
}
