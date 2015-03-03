package scalan.compilation.lms.cxx

import java.io.PrintWriter

import scala.virtualization.lms.internal._

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

  override def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    val tt = remap(sym.tp)
    if( "void" != tt )
      stream.println(super.quote(sym) + " = " + rhs + s"; /*emitAssignment(): ${sym.tp} ${sym} ${rhs}*/")
  }

  override def emitVarDecl(sym: Sym[Any]): Unit = {
    val tt = remap(sym.tp)
    if( "void" != tt )
      stream.println(tt + " " + quote(sym) + s"; /*emitVarDecl(): ${sym.tp} ${sym}*/")
  }

  override def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    emitConstruct(sym, rhs)
  }

  def emitConstruct(sym: Sym[Any], args: String*): Unit = {
    stream.println(s"${remap(sym.tp)} ${quote(sym)}(${args.mkString(",")}); /*emitConstruct(): ${sym.tp} ${sym}(${args.mkString(",")})*/")
  }

  def quoteMove(x: Exp[Any]): String = x match {
    case sym: Sym[_] =>
      if( isMoveable(sym) && moveableSyms.contains(sym) /*&& rightSyms.contains(sym)*/ )
        s"std::move(${super.quote(sym)})"
      else
        super.quote(sym)
    case _ =>
      super.quote(x)
  }

  override def emitValDef(sym: String, tpe: Manifest[_], rhs: String): Unit = {
    if (remap(tpe) != "void")
      stream.println("const " + remapWithRef(tpe) + " " + sym + " = " + rhs + ";")
//    else
//      stream.println(rhs + ";")
//      throw new IllegalArgumentException(s"can not emit void symbol, use emitUnitStatement() instead. $sym: $tpe = $rhs ")
  }

  def emitUnitStatement(rhs: String): Unit = {
    stream.println(rhs + ";")
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
          "#include <algorithm>\n" +
          "#include <unordered_map>\n" +
          "/*****************************************\n" +
          "  Emitting Generated Code                  \n" +
          "*******************************************/")
      emitFileHeader()

      val indargs = scala.Range(0, args.length).zip(args);
      stream.println(s"${sA} apply_$className(${indargs.map( p => s"const ${remapWithRef(p._2.tp)} ${quote(p._2)}").mkString(", ")} ) {")

      emitBlock(body)
      stream.println(s"return ${quoteMove(getBlockResult(body))};")

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
