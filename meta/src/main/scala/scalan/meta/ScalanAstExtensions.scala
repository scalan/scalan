package scalan.meta
/**
 * Created by slesarenko on 23/02/15.
 */
import ScalanAst._
import PrintExtensions._

trait ScalanAstExtensions {
  implicit class SMethodOrClassArgsOps(as: SMethodOrClassArgs) {
    def argNames = as.args.map(a => a.name)
    def argNamesAndTypes(config: CodegenConfig) = {
      as.args.map { arg =>
        if (config.isAlreadyRep || arg.isElemOrCont)
          s"${arg.name}: ${arg.tpe}"
        else
          s"${arg.name}: Rep[${arg.tpe}]"
      }
    }

    def argUnrepTypes(module: SEntityModuleDef, config: CodegenConfig) = {
      if (config.isAlreadyRep) {
        as.args.map(a => a.tpe.unRep(module, config).getOrElse {
          sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.")
        })
      } else as.args.map(_.tpe)
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
    def explicitReturnType = md.tpeRes.getOrElse(throw new IllegalStateException(s"Explicit return type required for method $this"))
    def declaration(config: CodegenConfig, includeOverride: Boolean) = {
      val typesDecl = md.tpeArgs.getBoundedTpeArgString(false, md.argSections)
      val argss = md.argSections.rep(sec => s"(${sec.argNamesAndTypes(config).rep()})", "")
      s"${includeOverride.opt("override ")}def ${md.name}$typesDecl$argss: $explicitReturnType"
    }
  }
}
