package scalan.compilation

import java.io.PrintWriter

import scalan.Scalan

/**
  * Created by slesarenko on 12/10/2017.
  */
class MockFileCodegen[IR <: Scalan](ctx: IR, config: CodegenConfig) extends FileCodegen[IR](ctx, config) {
  /** Name of language generated (used in error messages) */
  override def languageName: String = "mock"

  override def translateToSrc(arg: Any): String = super.translateToSrc(arg)

  override def emitHeader(graph: scalan.PGraph, functionName: String)
                         (implicit stream: PrintWriter): Unit = ???
  override def emitFooter(graph: scalan.PGraph, functionName: String)
                         (implicit stream: PrintWriter): Unit = ???
  override def functionHeader(sym: scalan.Sym,
                              args: List[scalan.Sym]): String = ???
  override def functionReturn(y: scalan.Sym): String = ???
  override def functionFooter(): Option[String] = ???
  override protected def simpleNode(sym: scalan.Sym,
                                    d: scalan.Def[_]): String = ???
  override def specialNumericLiteral(x: SpecialNumericValue,
                                     t: BaseNumericType): String = ???
  override implicit def srcStringHelper(sc: StringContext): SrcStringHelper = ???
}
