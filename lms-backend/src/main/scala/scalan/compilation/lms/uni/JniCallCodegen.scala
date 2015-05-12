package scalan.compilation.lms.uni

import java.io.PrintWriter

import scalan.compilation.lms.{BaseCodegen, LmsBackendFacade}
import scalan.compilation.lms.scalac.ScalaCommunityCodegen

/**
 * Created by adel on 5/14/15.
 */


class JniCallCodegen [BackendCake <: LmsBackendFacade](backend: BackendCake, nativeCodegen: BaseCodegen[BackendCake], packageName:String) extends ScalaCommunityCodegen(backend){
  //emitBlock()
  import IR._


  /*def emitSource[T : Manifest, R : Manifest](f: Exp[T] => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s = fresh[T]
    val jnis  = JNIType



    //val body = reifyBlock(f(s))
    emitSource(List(s), body, className, stream)
  } */


  def cppLibraryName(className:String) = "scalan_"+className.toLowerCase
  def cppFunctionName(className:String) = {
    val pp = packageName match {
      case p:String if p.length > 0 => p.replace("_", "_1").replace('.', '_')+"_"
      case _ => ""
    }
    "Java_" + pp + className.replace("_", "_1") + "_00024_apply"
  }

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {

    val sA = remap(manifest[A])

    val staticData = getFreeDataBlock(body)


    withStream(out) {
      stream.println("/*****************************************\n"+
        "  Emitting Scala Code                  \n"+
        "*******************************************/")
      packageName match {
        case p: String if p.length > 0 => stream.println("package " + p)
        case _ =>
      }
      /*
      override def emitFileHeader(): Unit = {
        super.emitFileHeader()
      }
      */
      emitFileHeader()

      stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tp).mkString(",")+")")+" extends (("+args.map(a => remap(a.tp)).mkString(", ")+")=>("+sA+")) {")
      stream.println("  def apply("+args.map(a => quote(a) + ":" + remap(a.tp)).mkString(", ")+"): "+sA +" = {")
      stream.println("    "+className+".apply("+args.map(a => quote(a)).mkString(", ")+") }")
      stream.println("}")
      stream.println("object "+className +" {")
      stream.println("  @native def apply("+args.map(a => quote(a) + ":" + remap(a.tp)).mkString(", ")+"): "+sA)
      stream.println("  System.loadLibrary(\"" + cppLibraryName(className) + "\")")
      stream.println("}")

      stream.println("/*****************************************\n"+
        "  End of Generated Code                  \n"+
        "*******************************************/")

      val test01Call: String = className match {
        case "test01_oneOp" =>
        """object ForTest01 {
         |  def main(arg:Array[String]) {
         |    	val x = new test01_oneOp
         |  	  println("test: 3+2 = " + x(3));
         |
         |  }
         |}
         |""".stripMargin
        case _ => ""
      }
      stream.println(test01Call)

    }

    staticData
  }

}
