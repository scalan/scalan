package scalan.meta
/**
 * Created by slesarenko on 23/02/15.
 */
import ScalanAst._

trait ScalanAstExtensions {
  import scalan.meta.PrintExtensions._

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
        as.args.map(a => a.tpe.unRep(module, config) match {
          case Some(t) => t
          case None => sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.")
        })
      } else as.args.map(_.tpe)
    }
  }

  implicit class STpeArgsOps(args: STpeArgs) {
    def getTpeArgDecls = args.map(_.declaration)
    def getTpeArgUse = args.map(_.name)

    def getTpeArgDeclString = getTpeArgDecls.opt(tyArgs => s"[${tyArgs.rep(t => t)}]")
    def getTpeArgUseString = getTpeArgUse.opt(tyArgs => s"[${tyArgs.rep(t => t)}]")

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
      args.opt(args =>
        s"[${
          args.rep(t => (if (t.isHighKind) s"${t.declaration}:Cont" else getElem(t)) + getWeakTypeTag(t))
        }]")
    }
  }

  implicit class STpeDefOps(td: STpeDef) {
    def emitTypeDecl = s"type ${td.name}${td.tpeArgs.getTpeArgDeclString} = Rep[${td.rhs}}]"
  }
}
