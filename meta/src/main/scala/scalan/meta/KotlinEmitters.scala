package scalan.meta

import scalan.meta.ScalanAst.{STraitCall, SClassArg, SValDef, STpeArgs, STpeExpr, STpeFunc, SExpr, STpeArg, STpeEmpty, SModuleDef, SClassDef, STpeTuple, STpeSingleton, STypeApply, STpeSelectFromTT, SClassArgs, STpeTypeBounds, STpePrimitive, SBodyItem, STpeAnnotated, STpeExistential}
import PrintExtensions._

trait Emitters {
  def genClass(c: SClassDef): String
}

case class GenCtx(module: SModuleDef)

class KotlinEmitters(implicit val ctx: GenCtx) extends Emitters {
  implicit def classDefEmitter(implicit ctx: GenCtx) = new CodeEmitter[SClassDef](genClass(_))

  implicit def bodyItemEmitter(implicit ctx: GenCtx) = new CodeEmitter[SBodyItem](genBodyItem(_))

  implicit def tpeExprEmitter(implicit ctx: GenCtx) = new CodeEmitter[STpeExpr](genTypeExpr(_))

  implicit def exprEmitter(implicit ctx: GenCtx) = new CodeEmitter[SExpr](emitExpr(_))

  def emit[T](x: T)(implicit e: CodeEmitter[T]): String = e(x)

  def genParents(ancestors: List[STypeApply])(implicit ctx: GenCtx): List[String] = {
    ancestors.map { a =>
      val tpeName = a.tpe.name
      val tpts = a.tpe.tpeSExprs.map(genTypeExpr)
      s"$tpeName${tpts.optList("<", ">")}"
    }
  }

  def genTypeArgs(tpeArgs: STpeArgs)
                 (implicit ctx: GenCtx): List[String] = tpeArgs.map(genTypeArg)

  def genTypeArg(arg: STpeArg)(implicit ctx: GenCtx): String = {
    val tpname = arg.name
    val tparams = arg.tparams.map(genTypeArg)
    s"$tpname${tparams.optList("<", ">")}"
  }

  def genClassArg(arg: SClassArg)(implicit ctx: GenCtx): String = {
    val tname = arg.name
    val tpt = genTypeExpr(arg.tpe)
    val valFlag = if (arg.valFlag) "val " else ""
    val overFlag = if (arg.overFlag) "override " else ""
    s"$overFlag$valFlag$tname: $tpt"
  }

  def genClassArgs(args: SClassArgs, implicitArgs: SClassArgs)
                  (implicit ctx: GenCtx): List[List[String]] = {
    val repArgs = args.args.map(genClassArg)
    val repImplArgs = implicitArgs.args.map(genClassArg)
    val repClassArgs = List[List[String]](repArgs, repImplArgs)
    repClassArgs.filterNot(_.isEmpty)
  }

  def genClass(c: SClassDef): String = {
    val className = c.name
    assert(c.selfType.isEmpty, "self types are not supported")
    val parents = genParents(c.ancestors)
    val bodyItems = genBody(c.body)
    val paramss = genClassArgs(c.args, c.implicitArgs)
    val absFlag = if (c.isAbstract) "abstract " else ""
    val tparams = c.tpeArgs.map(genTypeArg)
    val res =
      s"""
        |${absFlag}class $className${tparams.optList("<", ">")}(${paramss.rep()})${parents.optList(" : ", "")} {
        |  ${bodyItems.rep()}
        |}
      """.stripMargin
    res
  }

  def genBodyItem(x: SBodyItem)(implicit ctx: GenCtx): String = x match {
    case SValDef(n, tpe, isLazy, _, expr) =>
      val optType = tpe.opt(t => s": ${emit(t)}")
      val rhs = emit(expr)
      s"""val $n${optType} = $rhs"""
  }

  def genBody(body: List[SBodyItem])(implicit ctx: GenCtx): List[String] = body.map(genBodyItem)

  def genTypeSel(ref: String, name: String)(implicit ctx: GenCtx) = s"$ref.$name"

  def genTuple2(first: String, second: String)(implicit ctx: GenCtx): String = {
    val tpt = genTypeSel("kotlin", "Pair")
    val tpts = first :: second :: Nil
    s"$tpt${tpts.optList("<", ">")}>"
  }

  def genTuples(elems: List[STpeExpr])(implicit ctx: GenCtx): String = elems match {
    case x :: y :: Nil => genTuple2(genTypeExpr(x), genTypeExpr(y))
    case x :: xs => genTuple2(genTypeExpr(x), genTuples(xs))
    case Nil => throw new IllegalArgumentException("Tuple must have at least 2 elements.")
  }

  def genTypeExpr(tpeExpr: STpeExpr)(implicit ctx: GenCtx): String = tpeExpr match {
    case STpeEmpty() => ""
    case STpePrimitive(name: String, _) => name
    case STraitCall(name: String, tpeSExprs: List[STpeExpr]) =>
      val targs = tpeSExprs.map(genTypeExpr)
      s"$name${targs.optList("<", ">")}"
    case STpeTypeBounds(lo: STpeExpr, hi: STpeExpr) =>
      ??? //TODO  TypeBoundsTree(genTypeExpr(lo), genTypeExpr(hi))
    case STpeTuple(items: List[STpeExpr]) => genTuples(items)
    case STpeFunc(domain: STpeExpr, range: STpeExpr) =>
      val tDom = genTypeExpr(domain)
      val tRange = genTypeExpr(range)
      s"($tDom) -> $tRange"
    case _ => throw new NotImplementedError(s"genTypeExpr($tpeExpr)")
  }

  def emitExpr(x: SExpr)(implicit ctx: GenCtx): String = x match {
    case _ => ???
  }
}
