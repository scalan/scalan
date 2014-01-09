package scalan.codegen.emit.ast

import Types._
import Operator._
import scalan.codegen.emit.{CppCodeEmitter, Formatter}
import java.io.File

case class CompilationUnit( constants: List[Stat],
                            classes:   List[ClassDecl],
                            functions: List[FuncDecl],
                            mainFunc: Var) extends AbstractCompilationUnit(constants, classes, functions, mainFunc)

abstract class AbstractCompilationUnit(
  constants: List[Stat],
  classes:   List[ClassDecl],
  functions: List[FuncDecl],
  mainFunc: Var) extends AST {

  def calculateFunc = (functions find {
    f => f.name == mainFunc.name
  }).get

  def cppSource(inputFile: String = "input", outputFile: String = "output") : String =  {
    implicit val f = new Formatter
    val emitter = new CppCodeEmitter

    // #includes, typedefs, etc.
    f << "#include <iostream>\n"
    f << "#include <fstream>\n"
    f << "#include <cmath>\n"
    f << "#include <cfloat>\n"
    f << "#include <chrono>\n"
    f << "\n"
    f << "#include <algorithm>\n"
    f << "#include <vector>\n"
    f << "#include <queue>\n"
    f << "\n"
    f << "#include <unistd.h>\n"
    f << "\n"
    f << "#include \"dp.hpp\"\n"
    f << "\n"
    f << "#include \"typedefs.hpp\"\n"
    f << "\n"

    // Generate constants
    constants.foldLeft(";\n") {
      (sep, c) => f << emitter.emit(c) << sep; ";\n"
    }
    f << "\n"

    // Generate classes
    classes map {
      c => f << emitter.emit(c) << " \n\n"
    }

    // Generate functions
    functions.foldLeft("\n\n") {
      (sep, p) => f << emitter.emit(p) << sep; "\n\n"
    }

    /*
  val in: Graph = load[Graph]("in.txt")
  val res: PArray[Float] = func(Pair(in, 100))
  save[PArray[Float]](res, "out.txt")
  */
    /*
  f << "\t" << emitter.emit(calculateFunc.args.head.t) << " &in = load< " << emitter.emit(calculateFunc.args.head.t) << " >(\"" + inputFile + "\");\n"
  f << "\t" << emitter.emit(calculateFunc.returnType) << " res;\n"
  f << "\t" << calculateFunc.name << "(in, res);\n"
  f << "\tsave< " << emitter.emit(calculateFunc.returnType) << " >(res, \"" + outputFile + "\");\n"
  */

    f << "int main(int argc, char **argv) {\n"
    f << "\tconst char *inputFileName = \"" << inputFile << "\";\n"
    f << "\tconst char *outputFileName = \"" << outputFile << "\";\n"
    f << "\tint opt;\n"
    f << "\n"
    f << "\twhile ((opt = getopt(argc, argv, \"g:r:\")) != -1) {\n"
    f << "\t\tswitch (opt) {\n"
    f << "\t\t\tcase 'g':\n"
    f << "\t\t\t\tinputFileName = optarg;\n"
    f << "\t\t\t\tbreak;\n"
    f << "\t\t\tcase 'r':\n"
    f << "\t\t\t\toutputFileName = optarg;\n"
    f << "\t\t\t\tbreak;\n"
    f << "\t\t\tdefault:\n"
    f << "\t\t\t\tstd::cerr << \"Usage: \" << argv[0] << \" [-g INFILE] [-r OUTFILE]\" << std::endl;\n"
    f << "\t\t\t\tstd::cerr << \"  Options: \" << std::endl;\n"
    f << "\t\t\t\tstd::cerr << \"    -g file -- input file in a recursive format (default: \" << inputFileName << \")\" << std::endl;\n"
    f << "\t\t\t\tstd::cerr << \"    -r file -- output file in a recursive format (default: \" << outputFileName << \")\" << std::endl;\n"
    f << "\t\t\t\texit(EXIT_FAILURE);\n"
    f << "\t\t}\n"
    f << "\t}\n"
    f << "\n"
    f << "\tusing namespace std::chrono;\n"
    f << "\thigh_resolution_clock::time_point t1, t2, t3, t4, t5;\n"
    f << "\n"
    f << "\tt1 = high_resolution_clock::now();\n"
    f << "\n"
    f << "\tstd::istream* file_in = strncmp(inputFileName, \"-\", 2) ? new std::ifstream(inputFileName) : &std::cin;\n"
    f << "\tif (!file_in->good()) {\n"
    f << "\t\tstd::cerr << \"Error: cannot open input file `\" << inputFileName << '\\'' << std::endl;\n"
    f << "\t\texit(EXIT_FAILURE);\n"
    f << "\t}\n"
    f << "\n"

    val inType = calculateFunc.args.head.t match {
      case RefType(ConstType(t)) => t
      case ConstType(RefType(t)) => t
    }
    f << "\t" << emitter.emit(inType) << " in;\n"

    f << "\t(*file_in) >> in;\n"
    f << "\n"
    f << "\tt2 = high_resolution_clock::now();\n"
    f << "\n"

    val outType = calculateFunc.args.tail.head.t match {
      case RefType(t) => t
    }
    f << "\t" << emitter.emit(outType) << " out;\n"

    f << "\t" << calculateFunc.name << "(in, out);\n"
    f << "\n"
    f << "\tt3 = high_resolution_clock::now();\n"
    f << "\n"
    /* --- This is PageRank-specific: ---
  f << "\tstd::cout << \"number of iterations: \" << 100 - out.second.first << std::endl;\n"
  f << "\tstd::cout << \"norm_2^2: \" << out.second.second << std::endl;\n"
  f << "\n"
*/
    f << "\tstd::ostream* file_out = strncmp(outputFileName, \"-\", 2) ? new std::ofstream(outputFileName) : &std::cout;\n"
    f << "\tif (!file_out->good()) {\n"
    f << "\t\tstd::cerr << \"Error: cannot open output file `\" << outputFileName << '\\'' << std::endl;\n"
    f << "\t\texit(EXIT_FAILURE);\n"
    f << "\t}\n"
    f << "\n"
    f << "\t(*file_out) << out << std::endl;\n"
    f << "\n"
    f << "\tt4 = high_resolution_clock::now();\n"
    f << "\n"
    f << "\tstd::cout << \"elapsed time\" << std::endl;\n"
    f << "\tstd::cout << \"\\tinput: \" << duration_cast<duration<double>>(t2 - t1).count() << \" seconds\" << std::endl;\n"
    f << "\tstd::cout << \"\\tcomputation: \" << duration_cast<duration<double>>(t3 - t2).count() << \" seconds\" << std::endl;\n"
    f << "\tstd::cout << \"\\toutput: \" << duration_cast<duration<double>>(t4 - t3).count() << \" seconds\" << std::endl;\n"
    f << "\tstd::cout << \"\\toverall: \" << duration_cast<duration<double>>(t4 - t1).count() << \" seconds\" << std::endl;\n"
    f << "\n"

    f << "\treturn EXIT_SUCCESS;\n"
    f << "}\n"

    f.toString
  }


  def generateExecutable(dir: String, exeName: String, inputFile: String, outputFile: String, pathToDP: String = "/project/dp") = {
    val sourceCode = cppSource(inputFile, outputFile)

    // Save code to a file
    // TODO: How to implement crossplatform pathes
    writeToFile(dir + "/" + exeName + ".cpp", sourceCode)

    // Compile the file
    // ...

    val makefile: String =
      "ifndef PATHDP\n" + "\tPATHDP = %s\n".format(pathToDP) + "endif\n\n" +
      "include $(PATHDP)/dp.mk\n\n" +
      exeName + ": " + exeName + ".cpp $(PATHDP)/dp.hpp\n\n" +
      "clean:\n\t rm -f " + exeName + " *.o\n"
    writeToFile(dir + "/Makefile", makefile)
  }


  def generateFiles(dir: String, exeName: String) = {
    val inType = calculateFunc.args.head.t
    val outType = calculateFunc.args.last.t

    /*
    // dir/exeName.cpp
    #include "exeName.hpp"

    static void `calculateFunc.name`(const inType &in, outType &out) {
      // ...
    }

    void exeName(const inType &in, outType &out) {
      `calculateFunc.name`(in, out);
    }
    */
    def generateSourceFile(dir: String, exeName: String) {
      implicit val f = new Formatter
      val emitter = new CppCodeEmitter

      val file = dir + "/" + exeName + ".cpp"
      f << "// " + file + "\n"

      f << "#include <cfloat>\n"
      f << "#include \"" + exeName + ".hpp\"\n"
      f << "\n"

      // Generate constants
      constants.foldLeft(";\n"){ (sep, c) => f << emitter.emit(c) << sep ; ";\n" }
      f << "\n"

      // Generate classes
      classes.foldLeft("\n\n"){ (sep, p) => f << emitter.emit(p) << sep ; "\n\n" }
      f << "\n"

      // Generate functions
      functions.foldLeft("\n\n"){ (sep, p) => f << emitter.emit(p) << sep ; "\n\n" }
      f << "\n"

      f << "void " + exeName + "(" << emitter.emit(inType) << " in, " << emitter.emit(outType) << " out) {\n"
      f << "\t" + calculateFunc.name << "(in, out);\n"
      f << "}\n"
 
      writeToFile(file, f.toString)
    }

    /*
    // dir/exeName.hpp
    #ifndef __EXENAME_HPP__
    #define __EXENAME_HPP__
    #include "typedefs.hpp"
    #include "dp.hpp"
    void exeName(const inType &in, outType &out);
    #endif
    */
    def generateHeaderFile(dir: String, exeName: String) {
      implicit val f = new Formatter
      val emitter = new CppCodeEmitter

      val file = dir + "/" + exeName + ".hpp"
      f << "// " + file + "\n"

      val preprocessorMacro = ("__" + exeName + "_hpp__").toUpperCase
      f << "#ifndef " + preprocessorMacro + "\n"
      f << "#define " + preprocessorMacro + "\n"

      f << "#include \"typedefs.hpp\"\n"
      f << "#include \"dp.hpp\"\n"

      f << "void " + exeName + "(" << emitter.emit(inType) << " in, " << emitter.emit(outType) << " out);\n"

      f << "#endif\n"

      writeToFile(file, f.toString)
    }

    // We assume that `numa-runtime` is located at the same file system level as `scalan`
    // `numa-runtime` should be obtained as:
    // $> hg clone http://rrd-ds-lx03:8001 numa-runtime
    def generateMakefile(dir: String, exeName: String) {
      val makefile: String =
        "PATHDP = ../../../numa-runtime/dp\n\n" +
        "include $(PATHDP)/dp.mk\n\n" +
        exeName + "Main: "+ exeName +"Main.cpp " + exeName + ".cpp " + exeName + ".hpp" + " $(PATHDP)/typedefs.hpp $(PATHDP)/dp.hpp\n\n" +
        "clean:\n\t rm -f " + exeName + "Main" + " *.o\n"
      writeToFile("%s/%sMakefile".format(dir, exeName), makefile)
    }

    generateSourceFile(dir, exeName)
    generateHeaderFile(dir, exeName)
    generateMakefile(dir, exeName)
  }

  // TODO: How to implement crossplatform pathes in `file`
  def writeToFile(file: String, data: String): Unit = {
    val f = new File(file)
    f.getParentFile.mkdirs()
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream(file))
    stream.println(data)
    stream.close()
  }
}

