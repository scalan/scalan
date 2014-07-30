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

trait ProxyBase { self: Scalan =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Boolean = false)(implicit ct: ClassTag[Ops]): Ops

  def getStagedFunc(name: String): Rep[_] = {
    val clazz = this.getClass()
    val f = clazz.getDeclaredMethod(name)
    f.invoke(this).asInstanceOf[Rep[_]]
  }
}

trait ProxySeq extends ProxyBase { self: ScalanSeq =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Boolean)(implicit ct: ClassTag[Ops]): Ops = x
}

trait ProxyExp extends ProxyBase with BaseExp { self: ScalanStaged =>
  case class MethodCall[T](receiver: Exp[_], method: Method, args: List[AnyRef])(implicit leT: LElem[T]) extends Def[T] {
    def selfType = leT.value
    def uniqueOpId = s"$name:${method.getName}"
    override def mirror(t: Transformer) =
      MethodCall[T](t(receiver), method, args map {
        case a: Exp[_] => t(a)
        case a => a
      })
    override def self: Rep[T] = this
  }

  //  case class MethodCallLifted[T](receiver: Arr[_], method: Method, args: List[AnyRef])(implicit leT: LElem[T]) extends ArrayDef[T] {
  //    def selfType = leT.value
  //    def uniqueOpId = s"$name:{Lifted}${method.getName}"
  //    override def mirror(t: Transformer) =
  //      MethodCallLifted[T](t(receiver), method, args map {
  //        case a: Exp[_] => t(a)
  //        case a => a
  //      })
  //  }

  private val proxies = collection.mutable.Map.empty[(Rep[_], ClassTag[_]), AnyRef]
  private val objenesis = new ObjenesisStd

  override def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Boolean)(implicit ct: ClassTag[Ops]): Ops = {
    val proxy = proxies.getOrElseUpdate((x, ct), {
      val clazz = ct.runtimeClass
      val e = new Enhancer
      e.setClassLoader(clazz.getClassLoader)
      e.setSuperclass(clazz)
      e.setCallbackType(classOf[ExpInvocationHandler])
      val proxyClass = e.createClass().asSubclass(classOf[AnyRef])
      val proxyInstance = objenesis.newInstance(proxyClass).asInstanceOf[Factory]
      proxyInstance.setCallback(0, new ExpInvocationHandler(x, forceInvoke))
      proxyInstance
    })
    proxy.asInstanceOf[Ops]
  }

  var invokeEnabled = false

  private def hasFuncArg(args: Array[AnyRef]): Boolean =
    args.exists {
      case f: Function0[_] => true
      case f: Function1[_, _] => true
      case f: Function2[_, _, _] => true
      case _ => false
    }

  // stack of receivers for which MethodCall nodes should be created by InvocationHandler
  var methodCallReceivers = Set.empty[Exp[_]]

  class ExpInvocationHandler(receiver: Exp[_], forceInvoke: Boolean) extends InvocationHandler {
    def canInvoke(m: Method, d: Def[_]) = m.getDeclaringClass.isAssignableFrom(d.getClass)
    def shouldInvoke(args: Array[AnyRef]) = invokeEnabled || forceInvoke || hasFuncArg(args)

    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
      val args = if (_args == null) scala.Array.empty[AnyRef] else _args
      receiver match {
        // call method of the node when it's allowed
        case Def(d) if (canInvoke(m, d) && shouldInvoke(args)) =>
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
        case e: Elem[a] =>
          MethodCall[a](receiver, m, args.toList)(Lazy(e))
      }
    }

    // used when a method isn't defined for zero node to determine the type
    // see e.g. OptionOps.get
    class ElemException[A](message: String)(implicit val element: Elem[A]) extends StagingException(message, List.empty)

    def getResultElem(m: Method, args: Array[AnyRef]): Elem[_] = {
      val e = receiver.elem
      val zero = e.defaultRepValue
      val Def(zeroNode) = zero
      try {
        val res = m.invoke(zeroNode, args: _*)
        res match {
          case s: Exp[_] => s.elem
          case other => !!!(s"Result of staged method call must be an Exp, but got $other")
        }
      } catch {
        case e: InvocationTargetException =>
          e.getCause match {
            case e1: ElemException[_] => e1.element
            case _ => throw e
          }
      }
    }
  }
}
