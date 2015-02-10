package scalan.compilation.lms.cxx

import java.io.PrintWriter

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

  def isMoveable( sym: Exp[Any]): Boolean = {
    if (sym.tp <:< Manifest.AnyRef)
      true
    else
      false
  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym,rhs) =>
        rightSyms.clear
        rightSyms ++= syms(rhs)
        if (isMoveable(sym) && syms(rhs).find(moveableSyms.contains) != None) // derived from moveable
          moveableSyms += sym
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

  override def emitSource[A: Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val sA = remap(manifest[A])

    //      val staticData = getFreeDataBlock(body)

    withStream(out) {
      stream.println(
          "#include <vector>\n" +
          "#include <cstdlib>\n" +
          "#include <functional>\n" +
          "/*****************************************\n" +
          "  Emitting Generated Code                  \n" +
          "*******************************************/")
      emitFileHeader()

      val indargs = scala.Range(0, args.length).zip(args);
      stream.println(s"${sA} apply(${indargs.map( p => s"${remapWithRef(p._2.tp)} ${quote(p._2)}").mkString(", ")} ) {")

      emitBlock(body)
      stream.println(s"return ${quote(getBlockResult(body))};")

      stream.println("}")
      stream.println("/*****************************************\n" +
        "  End of Generated Code                  \n" +
        "*******************************************/")
    }

    Nil
  }
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
      case TP(sym, rhs) =>
        rhs match {
          case Forward(x) =>
            if( isMoveable(sym) )
              moveableSyms += sym
          case _ =>
            ()
        }
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }
}
