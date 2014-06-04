/**
 * Shamelessly taken from https://github.com/namin/lms-sandbox
 */
package scalan

import java.lang.reflect.Method

import scala.reflect.ClassTag

import org.objenesis.ObjenesisStd

import net.sf.cglib.proxy.Enhancer
import net.sf.cglib.proxy.Factory
import net.sf.cglib.proxy.InvocationHandler
import scalan.common.Lazy
import scalan.staged.BaseExp

trait ProxyBase { self: Scalan =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Option[Boolean] = None)(implicit ct: ClassTag[Ops]): Ops

  def getStagedFunc(name: String): Rep[_] = {
    val clazz = this.getClass()
    val f = clazz.getDeclaredMethod(name)
    f.invoke(this).asInstanceOf[Rep[_]]
  }
}

trait ProxySeq extends ProxyBase { self: ScalanSeq =>
  def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Option[Boolean] = None)(implicit ct: ClassTag[Ops]): Ops = x
}

trait ProxyExp extends ProxyBase with BaseExp { self: ScalanStaged =>

  case class MethodCall[T](receiver: Exp[Any], method: Method, args: List[AnyRef])(implicit leT: LElem[T]) extends Def[T] {
    def selfType = leT.value
    def uniqueOpId = s"$name:${method.getName}"
    override def mirror(t: Transformer) =
      MethodCall[T](t(receiver), method, args map { case a: Exp[_] => t(a) case a => a })
    override def self: Rep[T] = { this }
  }

  //TODO
  //  case class MethodCallLifted[T](receiver: PA[Any], method: jreflect.Method, args: List[AnyRef])
  //                                (implicit val elem: Elem[T]) extends StagedArrayBase[T] {
  //    override def mirror(t: Transformer) =
  //      MethodCallLifted[T](t(receiver), method, args map { case a: Exp[_] => t(a) case a => a })
  //  }

  private val proxies = collection.mutable.Map.empty[(Rep[_], ClassTag[_]), AnyRef]
  private val objenesis = new ObjenesisStd

  override def proxyOps[Ops <: AnyRef](x: Rep[Ops], forceInvoke: Option[Boolean] = None)(implicit ct: ClassTag[Ops]): Ops = {
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

  private def hasFuncArg(args: Array[AnyRef]): Boolean = args.exists(_.isInstanceOf[(_) => _])

  // stack of receivers for which MethodCall nodes should be created by InvocationHandler
  var methodCallReceivers = List.empty[Exp[Any]]

  class ExpInvocationHandler(receiver: Exp[Any], forceInvoke: Option[Boolean]) extends InvocationHandler {

    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
      val args = if (_args == null) scala.Array.empty[AnyRef] else _args
      receiver match {
        case Def(d) => { // call method of the node
          val nodeClazz = d.getClass
          m.getDeclaringClass.isAssignableFrom(nodeClazz) && (invokeEnabled || forceInvoke.getOrElse(false)) && (!hasFuncArg(args)) match {
            case true =>
              val res = m.invoke(d, args: _*)
              res
            case _ => invokeMethodOfVar(m, args)
          }
        }
        case _ => invokeMethodOfVar(m, args)
      }
    }

    def invokeMethodOfVar(m: Method, args: Array[AnyRef]) = {
      createMethodCall(m, args)
//      /* If invoke is enabled or current method has arg of type <function> - do not create methodCall */
//      methodCallReceivers.contains(receiver) || (!((invokeEnabled || forceInvoke.getOrElse(false)) || hasFuncArg(args))) match {
//        case true =>
//          createMethodCall(m, args)
//        case _ => receiver.elem match {
//          case ve: ViewElem[_, _] =>
//            //val e = getRecieverElem
//            val iso = ve.iso
//            methodCallReceivers = methodCallReceivers :+ receiver
//            val wrapper = iso.to(iso.from(receiver))
//            methodCallReceivers = methodCallReceivers.tail
//            val Def(d) = wrapper
//            val res = m.invoke(d, args: _*)
//            res
//          case _ =>
//            createMethodCall(m, args)
//        }
//      }
    }

    def createMethodCall(m: Method, args: Array[AnyRef]): Exp[_] = {
      getResultElem(m, args) match {
        case e: Elem[a] =>
          val resultElem = Lazy(e)
          reifyObject(MethodCall[a](
            receiver, m, args.toList)(resultElem))(resultElem)
      }
    }

    def getResultElem(m: Method, args: Array[AnyRef]): Elem[_] = {
      val e = receiver.elem
      val zero = e.defaultRepValue
      val Def(zeroNode) = zero
      val res = m.invoke(zeroNode, args: _*)
      res match {
        case s: Exp[_] => s.elem
        case other => ???(s"don't know how to get result elem for $other")
      }
    }

  }
}