package scalan.compilation.lua

import java.io.File

import org.luaj.vm2.{LuaTable, LuaValue}
import org.luaj.vm2.lib.jse.JsePlatform

import scala.collection.mutable
import scalan.ScalanDslExp
import scalan.compilation.{Compiler, GraphVizConfig}

class LuaCompiler[+ScalanCake <: ScalanDslExp](_scalan: ScalanCake) extends Compiler(_scalan) {
  import scalan._

  val codegen = new LuaCodegen[scalan.type](scalan)

  type CompilerConfig = Unit

  def defaultCompilerConfig = ()

  type CustomCompilerOutput = LuaValue

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]): CustomCompilerOutput = {
    val file = codegen.emitSourceFile(graph, functionName, sourcesDir)
    val globals = JsePlatform.standardGlobals()
    globals.get("dofile").call(LuaValue.valueOf(file.getAbsolutePath))
    val fun = globals.get(functionName)
    assert(!fun.isnil(), s"Global function $functionName is expected to be present in script ${file.getAbsolutePath}")
    fun
  }

  protected def toLuaValue(x: Any, eX: Elem[_]): LuaValue = eX match {
    case UnitElement => LuaValue.NIL
    case BooleanElement => LuaValue.valueOf(x.asInstanceOf[Boolean])
    case IntElement => LuaValue.valueOf(x.asInstanceOf[Int])
    case DoubleElement => LuaValue.valueOf(x.asInstanceOf[Double])
    case LongElement => LuaValue.valueOf(x.asInstanceOf[Long].toDouble)
    case FloatElement => LuaValue.valueOf(x.asInstanceOf[Float].toDouble)
    case StringElement => LuaValue.valueOf(x.asInstanceOf[String])
    case el: ArrayElem[_] =>
      val eItem = el.eItem
      LuaValue.listOf(x.asInstanceOf[Array[_]].map(toLuaValue(_, eItem)))
    case PairElem(eFst, eSnd) =>
      val pair = x.asInstanceOf[(_, _)]
      val fst = toLuaValue(pair._1, eFst)
      val snd = toLuaValue(pair._2, eSnd)
      LuaValue.listOf(Array(fst, snd))
    case _ => !!!(s"Can't convert $x of type ${eX.name} to a Lua value")
  }

  // should check type before conversion?
  protected def fromLuaValue[A](lv: LuaValue, eA: Elem[A]): A = (eA match {
    case UnitElement => ()
    case BooleanElement => lv.toboolean()
    case IntElement => lv.toint()
    case DoubleElement => lv.todouble()
    case LongElement => lv.tolong()
    case FloatElement => lv.tofloat()
    case StringElement => lv.tostring()
    case el: ArrayElem[_] =>
      val eItem = el.eItem
      val table = lv.asInstanceOf[LuaTable]
      val len = table.rawlen()
      val res = Array.ofDim(len)(eItem.classTag)
      for (i <- 0 until len) {
        res(i) = fromLuaValue(table.get(i + 1), eItem)
      }
      res
    case PairElem(eFst, eSnd) =>
      val fst = fromLuaValue(lv.get(1), eFst)
      val snd = fromLuaValue(lv.get(2), eSnd)
      (fst, snd)
    case _ => !!!(s"Can't convert LuaValue $lv to JVM value of type ${eA.name}")
  }).asInstanceOf[A]

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    val luaInput = toLuaValue(input, compilerOutput.common.eInput)
    val luaOutput = compilerOutput.custom.call(luaInput)
    fromLuaValue(luaOutput, compilerOutput.common.eOutput)
  }
}
