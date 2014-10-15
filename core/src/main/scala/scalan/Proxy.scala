/**
 * Shamelessly taken from https://github.com/namin/lms-sandbox
 */
package scalan

import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method

import scala.reflect.ClassTag

import org.objenesis.ObjenesisStd

import net.sf.cglib.proxy.Enhancer
import net.sf.cglib.proxy.Factory
import net.sf.cglib.proxy.InvocationHandler
import scalan.common.Lazy
import scalan.staged.BaseExp
import scalan.util.ScalaNameUtil

trait Proxy { self: Scalan =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Boolean = false)(implicit ct: ClassTag[Ops]): Ops

  def getStagedFunc(name: String): Rep[_] = {
    val clazz = this.getClass
    val f = clazz.getDeclaredMethod(name)
    f.invoke(this).asInstanceOf[Rep[_]]
  }
}

trait ProxySeq extends Proxy { self: ScalanSeq =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Boolean = false)(implicit ct: ClassTag[Ops]): Ops = x
}

trait ProxyExp extends Proxy with BaseExp { self: ScalanExp =>
  case class MethodCall[T](receiver: Exp[_], method: Method, args: List[AnyRef])(implicit selfType: Elem[T]) extends BaseDef[T] {
    def uniqueOpId = s"$name:${method.getName}"
    override def mirror(t: Transformer) =
      MethodCall[T](t(receiver), method, args map {
        case a: Exp[_] => t(a)
        case a => a
      })
  }

  private val proxies = collection.mutable.Map.empty[(Rep[_], ClassTag[_]), AnyRef]
  private val objenesis = new ObjenesisStd

  override def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Boolean = false)(implicit ct: ClassTag[Ops]): Ops =
    x match {
      case Def(d) => d match {
        case Const(c) => c
        case _ =>
          getProxy(x, ct, forceInvoke)
      }
      case _ =>
        getProxy(x, ct, forceInvoke)
    }

  private def getProxy[Ops](x: Rep[Ops], ct: ClassTag[Ops], forceInvoke: Boolean) = {
    val proxy = proxies.getOrElseUpdate((x, ct), {
      val clazz = ct.runtimeClass
      val e = new Enhancer
      e.setClassLoader(clazz.getClassLoader)
      e.setSuperclass(clazz)
      e.setCallbackType(classOf[ExpInvocationHandler[_]])
      val proxyClass = e.createClass().asSubclass(classOf[AnyRef])
      val proxyInstance = objenesis.newInstance(proxyClass).asInstanceOf[Factory]
      proxyInstance.setCallback(0, new ExpInvocationHandler(x, forceInvoke))
      proxyInstance
    })
    proxy.asInstanceOf[Ops]
  }

  // FIXME this is a hack, this should be handled in Passes
  // The problem is that rewriting in ProgramGraph.transform is non-recursive
  // We need some way to make isInvokeEnabled local to graph
  type InvokeTester = (Def[_], Method) => Boolean

  private var invokeTesters: Set[InvokeTester] = Set.empty

  def isInvokeEnabled(d: Def[_], m: Method) = invokeTesters.exists(_(d, m))

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

  // stack of receivers for which MethodCall nodes should be created by InvocationHandler
  protected var methodCallReceivers = Set.empty[Exp[_]]

  class ExpInvocationHandler[T](receiver: Exp[T], forceInvoke: Boolean) extends InvocationHandler {
    override def toString = s"ExpInvocationHandler(${receiver.toStringWithDefinition})"

    def shouldInvoke(d: Def[_], m: Method, args: Array[AnyRef]) =
      m.getDeclaringClass.isAssignableFrom(d.getClass) &&
        (isInvokeEnabled(d, m) || forceInvoke || hasFuncArg(args))

    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
      val args = if (_args == null) scala.Array.empty[AnyRef] else _args
      receiver match {
        // call method of the node when it's allowed
        case Def(d) if shouldInvoke(d, m, args) =>
          val res = m.invoke(d, args: _*)
          res
        case _ => invokeMethodOfVar(m, args)
      }
    }

    def invokeMethodOfVar(m: Method, args: Array[AnyRef]) = {
      createMethodCall(m, args)
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

    def createMethodCall(m: Method, args: Array[AnyRef]): Exp[_] = {
      getResultElem(m, args) match {
        case e: Elem[a] => MethodCall[a](receiver, m, args.toList)(e)
      }
    }

    def getResultElem(m: Method, args: Array[AnyRef]): Elem[_] = {
      val e = receiver.elem
      val zero = e.defaultRepValue
      val Def(zeroNode) = zero
      try {
        val res = m.invoke(zeroNode, args: _*)
        res match {
          case s: Exp[_] => s.elem
          case other => !!!(s"Staged method call ${ScalaNameUtil.cleanScalaName(m.toString)} must return an Exp, but got $other")
        }
      } catch {
        case e: InvocationTargetException =>
          e.getCause match {
            case e1: ElemException[_] => e1.element
            case _ => throw e
          }
      }
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
}
