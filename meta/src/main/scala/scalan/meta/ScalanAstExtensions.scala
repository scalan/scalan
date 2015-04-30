package scalan.meta
/**
 * Created by slesarenko on 23/02/15.
 */
import ScalanAst._

trait ScalanAstExtensions {
  import scalan.meta.PrintExtensions._

  implicit class SMethodOrClassArgsOps(as: SMethodOrClassArgs) {
    def argNames = as.args.map(a => a.name)
    def argNamesAndTypes = as.args.map(a => s"${a.name}: ${a.tpe}")

    def argUnrepTypes(module: SEntityModuleDef/*, config: CodegenConfig*/) =
      as.args.map(a => a.tpe.unRep(module/*, config.entityTypeSynonyms*/) match {
        case Some(t) => t
        case None => sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.")
      })
  }

  implicit class STpeArgsOps(args: STpeArgs) {
    def getTpeArgDecls = args.map(_.declaration)
    def getTpeArgUse = args.map(_.name)

    def getTpeArgDeclString = getTpeArgDecls.opt(tyArgs => s"[${tyArgs.rep(t => t)}]")
    def getTpeArgUseString = getTpeArgUse.opt(tyArgs => s"[${tyArgs.rep(t => t)}]")

    def getBoundedTpeArgString(withTags: Boolean = false) =
      args.opt(args =>
        s"[${args.rep(t => (if (t.isHighKind) s"${t.declaration}:Cont" else s"${t.name}:Elem") +
          withTags.opt(":WeakTypeTag"))}]")
  }

  implicit class STpeDefOps(td: STpeDef) {
    def emitTypeDecl = s"type ${td.name}${td.tpeArgs.getTpeArgDeclString} = Rep[${td.rhs}}]"
  }
}
