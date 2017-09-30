package scalan.meta

/**
  * Created by slesarenko on 23/02/15.
  */
import ScalanAst._
import PrintExtensions._

object ScalanAstExtensions {

  implicit class STpeExprOps(t: STpeExpr) {
    def toIdentifier: String = {
      def mkId(name: String, parts: Seq[STpeExpr]) =
        (name +: parts).mkString("_")

      t match {
        case STpePrimitive(name, _) => name
        case STraitCall(name, args) => mkId(name, args)
        case STpeTuple(items) => mkId("Tuple", items)
        //case STpeSum(items) => mkId("Sum", items)
        case STpeFunc(domain, range) => mkId("Func", Seq(domain, range))
        case STpeTypeBounds(lo, hi) => mkId("Bounds", Seq(lo, hi))
        case _ => t.name
      }
    }
  }

  implicit class SMethodOrClassArgOps(arg: SMethodOrClassArg) {
    def unrepType(module: SModuleDef) =
      if (module.isVirtualized) arg.tpe.unRep(module, module.isVirtualized)
      else arg.tpe
  }

  implicit class SMethodOrClassArgsOps(as: SMethodOrClassArgs) {
    def argNames = as.args.map(a => a.name)

    def argNamesAndTypes(config: CodegenConfig) = {
      as.args.map { arg =>
        if (config.isVirtualized || arg.isTypeDesc)
          s"${arg.name}: ${arg.tpe}"
        else
          s"${arg.name}: Rep[${arg.tpe}]"
      }
    }

    def argUnrepTypes(module: SModuleDef, isVirtualized: Boolean) = {
      if (isVirtualized) {
        as.args.map({ a =>
          val res = a.tpe.unRep(module, isVirtualized)
          res.getOrElse { sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.") }
        })
      }
      else
        as.args.map(_.tpe)
    }
  }

  implicit class STpeArgsOps(args: STpeArgs) {
    def decls = args.map(_.declaration)

    def names = args.map(_.name)

    def declString = decls.asTypeParams()

    def useString = names.asTypeParams()

    def getBoundedTpeArgString(withTags: Boolean = false, methodArgs: List[SMethodArgs] = Nil) = {
      def getElem(tpeArg: STpeArg) = {
        if (tpeArg.hasElemBound(methodArgs)) s"${tpeArg.name}"
        else s"${tpeArg.name}:Elem"
      }

      def getCont(tpeArg: STpeArg) = {
        if (tpeArg.hasContBound(methodArgs)) s"${tpeArg.declaration}"
        else s"${tpeArg.declaration}:Cont"
      }

      def getWeakTypeTag(tpeArg: STpeArg) = {
        if (tpeArg.hasWeakTypeTagBound(methodArgs)) ""
        else withTags.opt(":WeakTypeTag")
      }

      args.asTypeParams { t =>
        (if (t.isHighKind) getCont(t) else getElem(t)) + getWeakTypeTag(t)
      }
    }
  }

  implicit class STpeDefOps(td: STpeDef) {
    def declaration = s"type ${td.name}${td.tpeArgs.declString} = Rep[${td.rhs}}]"
  }

  implicit class SMethodDefOps(md: SMethodDef) {
    def explicitReturnType(config: CodegenConfig): String = {
      def error = throw new IllegalStateException(s"Explicit return type required for method $md")

      val tRes = md.tpeRes.getOrElse(error)
      if (config.isVirtualized) tRes.toString
      else s"Rep[$tRes]"
    }

    def declaration(config: CodegenConfig, includeOverride: Boolean) = {
      val typesDecl = md.tpeArgs.declString
      val argss = md.argSections.rep(sec => s"(${sec.argNamesAndTypes(config).rep()})", "")
      s"${includeOverride.opt("override ")}def ${md.name}$typesDecl$argss: ${explicitReturnType(config)}"
    }
  }

  implicit class SModuleOps(module: SModuleDef) {
    def selfTypeString(suffix: String) =
      module.selfType.opt(t => s"self: ${t.tpe}${suffix} =>")
  }

}
