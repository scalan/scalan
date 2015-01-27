package scalan.compilation.lms

import scala.virtualization.lms.internal._

/**
 * Created by zotov on 12/8/14.
 */
trait CXXCodegen extends CLikeCodegen {
  val IR: Expressions
  import IR._

  trait size_t
  trait auto_t

  trait Noref[T]
  def norefManifest[T: Manifest]( m: Manifest[T]): Manifest[_] = Manifest.classType(classOf[Noref[_]], m)

  val moveableSyms: scala.collection.mutable.Set[Exp[_]] = scala.collection.mutable.Set.empty
  val rightSyms: scala.collection.mutable.Set[Exp[_]] = scala.collection.mutable.Set.empty

  override def getBlockResult[A](s: Block[A]): Exp[A] = {
    val res = super.getBlockResult(s)
    rightSyms.clear
    rightSyms += res
    res
  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym,rhs) =>
        rightSyms.clear
        rightSyms ++= syms(rhs)
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[Noref[_]] =>
        remap(m.typeArguments(0))
      case c if c.isArray =>
        val itemType = m.typeArguments(0)
        val ts = remap(itemType)
          s"std::vector<${ts}>"
      case c if c == classOf[size_t] =>
        "size_t"
      case c if c == classOf[auto_t] =>
        "auto"
      case c if c == classOf[scala.Tuple2[_,_]] =>
        s"std::pair<${remap(m.typeArguments(0))},${remap(m.typeArguments(1))}>"
      case _ =>
        super.remap(m)
    }
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if( moveableSyms.contains(sym) )
      emitValDef(quote(sym), norefManifest(sym.tp), rhs)
    else
      emitValDef(quote(sym), sym.tp, rhs)
  }

  override def quote(x: Exp[Any]) = x match {
    case sym: Sym[_] =>
      if( moveableSyms.contains(sym) && rightSyms.contains(sym) )
        s"std::move(${super.quote(sym)})"
      else super.quote(sym)
    case _ =>
      super.quote(x)
  }

  override def emitValDef(sym: String, tpe: Manifest[_], rhs: String): Unit = {
    if (remap(tpe) != "void")
      stream.println(remapWithRef(tpe) + " " + sym + " = " + rhs + ";")
  }

  override def remapWithRef[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[Noref[_]] =>
        remap(m.typeArguments(0))
      case c if c == classOf[size_t] =>
        remap(m)
      case _ =>
        super.remapWithRef(m)
    }
  }

  override def addRef() = "&"
}

trait CXXFatCodegen extends CXXCodegen with CLikeFatCodegen {
  val IR: Expressions with FatExpressions with Effects
  import IR._

  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef): Unit = {
    super.emitFatNode(sym, rhs)
  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TTP(lhs,mhs,rhs) =>
        if( syms(rhs).contains(moveableSyms.contains _) )
          moveableSyms ++= lhs
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }
}