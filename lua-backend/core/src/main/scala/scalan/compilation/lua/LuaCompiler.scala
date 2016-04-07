package scalan.compilation.lua

import java.io.File

import org.luaj.vm2.{LuaTable, LuaValue}
import org.luaj.vm2.lib.jse.JsePlatform

import scala.collection.mutable
import scalan.{ScalanDslExp, ScalanDslStd}
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
    val fullPath = file.getAbsolutePath
    try {
      globals.get("dofile").call(LuaValue.valueOf(fullPath))
    } catch {
      // LuaJ doesn't show the full path otherwise
      case e: Exception => !!!(s"Failed to read or compile $fullPath", e)
    }
    val fun = globals.get(functionName)
    assert(!fun.isnil(), s"Global function $functionName is expected to be present in script $fullPath")
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
    case StructElem(_, fields) =>
      val struct = x.asInstanceOf[scalanStd.Struct]
      val namedValues = Array.ofDim[LuaValue](fields.length * 2)
      fields.zipWithIndex.foreach { case ((key, elem), i) =>
        namedValues(i * 2) = LuaValue.valueOf(key)
        val value = struct.fields(i)._2
        namedValues(i * 2 + 1) = toLuaValue(value, elem)
      }
      LuaValue.tableOf(namedValues)
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
    case StructElem(tag, elemFields) =>
      val tagStd = tag.asInstanceOf[scalanStd.StructTag[A with scalanStd.Struct]]
      val structFields = elemFields.map { case (key, elem) =>
        val value = fromLuaValue(lv.get(key), elem)
        (key, value)
      }
      scalanStd.struct(tagStd, structFields)
    case _ => !!!(s"Can't convert LuaValue $lv to JVM value of type ${eA.name}")
  }).asInstanceOf[A]

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    val luaInput = toLuaValue(input, compilerOutput.common.eInput)
    val luaOutput = compilerOutput.custom.call(luaInput)
    fromLuaValue(luaOutput, compilerOutput.common.eOutput)
  }

  protected[this] lazy val scalanStd = new ScalanDslStd
}
