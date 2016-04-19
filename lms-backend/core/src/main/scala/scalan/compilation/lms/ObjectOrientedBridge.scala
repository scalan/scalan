package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.compilation.language._

trait ObjectOrientedBridge extends LmsBridge with MethodMappingDSL {
  import scalan._
  import language.{Library, TypeT, MethodT}

  def mappedMethod(method: Method): Option[(Library, TypeT, MethodT)] =
    methodReplaceConf.map(_.getMethod(method)).collectFirst { case Some(x) => x }

  def mappedType(m: Manifest[_]): Option[(Library, TypeT)] =
    methodReplaceConf.map(_.getType(m)).collectFirst { case Some(x) => x }

  val language: LanguageMapping

  lazy val methodReplaceConf = mappingsFor(language)

  def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] = {
    elemToManifest(returnType) match {
      case mA: Manifest[a] =>
        val numArgs = args.length

        def checkArg[A](iAdj: Adjusted[Int])(f: PartialFunction[Any, A]) = iAdj.value match {
          case -1 =>
            f.applyOrElse(receiver, { x: Any => !!!(s"Unexpected receiver for method $method: $x") })
          case i =>
            if (i < numArgs) {
              f.applyOrElse(args(i), { x: Any =>
                val numStr = i match {
                  case 1 => "1st"
                  case 2 => "2nd"
                  case 3 => "3rd"
                  case _ => s"${i}th"
                }
                !!!(s"Unexpected $numStr argument for method $method: $x")
              })
            } else {
              // TODO show the method better
              !!!(s"Mapping error for method $method: got $numArgs arguments, must be at least ${i + 1}.")
            }
        }

        mappedMethod(method) match {
          case Some((_, _, methodT)) if methodT.isInternal =>
            val lmsArgs = methodT.argOrder.map(checkArg(_) {
              case e: Exp[_] =>
                m.symMirrorUntyped(e)
            })

            val lmsMethod = lmsMemberByName(methodT.mappedName).asMethod
            lmsMirror.reflectMethod(lmsMethod).apply(lmsArgs: _*).asInstanceOf[lms.Exp[_]]

          case Some((libraryT, typeT, methodT)) =>
            val mappedReceiver = checkArg(methodT.receiverIndex) {
              case e: Exp[_] => e
            }
            // TODO make sure header/jar is added
            val lmsReceiver = if (mappedReceiver.isCompanion || methodT.isStatic) {
              lms.Static(staticReceiverString(libraryT, typeT))
            } else {
              lms.Instance(m.symMirrorUntyped(mappedReceiver))
            }
            // FIXME take order into account! Account for differences between C++ and Scala as well

            val typeArgs = args.collect { case elem: Elem[_] => elemToManifest(elem) }
            val lmsArgs = methodT.argOrder.map(checkArg(_) {
              case e: Exp[_] =>
                m.symMirrorUntyped(e)
            })
            lms.methodCall[a](lmsReceiver, lms.Pure, methodT.mappedName, typeArgs, lmsArgs: _*)(mA)

          case None =>
            val lmsReceiver = if (receiver.isCompanion) {
              mappedType(elemToManifest(receiver.elem)) match {
                case Some((library, tpe)) =>
                  lms.Static(staticReceiverString(library, tpe))
                case None =>
                  // TODO handle this case, at least for TypeWrapper
                  !!!(s"Add mapping for unmapped companion ${receiver.toStringWithDefinition}", receiver)
              }
            } else {
              lms.Instance(m.symMirrorUntyped(receiver))
            }
            val typeArgs = args.collect { case elem: Elem[_] => elemToManifest(elem) }
            val lmsArgs = args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
            lms.methodCall[a](lmsReceiver, lms.Pure, method.getName, typeArgs, lmsArgs: _*)(mA)
        }
    }
  }

  // TODO move to codegen
  def staticReceiverString(library: Library, tpe: TypeT): String

  def newObj[A](m: Manifest[A], args: Seq[Any]): lms.Exp[A] = {
    val name = mappedType(m) match {
      case Some((_, tpe)) =>
        tpe.mappedName
      case _ =>
        m.runtimeClass.getName
    }
    lms.newObj[A](name, args, true)(m)
  }

  override protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case mc@MethodCall(receiver, method, args, _) =>
      val exp = (isWrapperElem(receiver.elem) && isValueAccessor(method)) match {
        case true  => m.symMirror[T](receiver)
        case false => transformMethodCall[T](m, receiver, method, args, mc.selfType.asInstanceOf[Elem[T]])
      }

      m.addSym(sym, exp)

    case lr@NewObject(eA, args, _) =>
      elemToManifest(eA) match {
        case mA: Manifest[a] =>
          // TODO handle case when some of params are functions
          val lmsArgs = args.map(mapParam(m, _, false))
          val exp = newObj[a](mA, lmsArgs)
          m.addSym(sym, exp)
      }

    case _ => super.transformDef(m, g, sym, d)
  }
}
