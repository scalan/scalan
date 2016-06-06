package scalan.compilation

import scalan.{ScalanDslExp, BaseNestedTests}

// run with -Dscalan.plugins.extraClassPath=/home/aromanov/.ivy2/cache/org.luaj/luaj-jse/jars/luaj-jse-3.0.1.jar:/home/aromanov/IdeaProjects/scalan-lite/lua-backend/core/target/scala-2.11/classes:/home/aromanov/IdeaProjects/scalan-lite/lms-backend/core/target/scala-2.11/classes:/home/aromanov/.ivy2/cache/org.scala-lang.virtualized/scala-library/jars/scala-library-2.11.2.jar:/home/aromanov/.ivy2/cache/org.scala-lang.virtualized/scala-compiler/jars/scala-compiler-2.11.2.jar:/home/aromanov/.ivy2/cache/org.scala-lang.lms/lms-core_2.11/jars/lms-core_2.11-0.9.1-SNAPSHOT.jar
// replace /home/aromanov/... with correct directories. Note that JVM won't resolve ~ as home directory!
// This is only required
class KernelTests extends BaseNestedTests {

  describe("can create and launch simple kernel") {
    // On CI the backends aren't compiled yet when this runs
    pendingOnCI()

    val s = new ScalanDslExp {}
    import s._
    val incFun: Rep[Array[Int] => Array[Int]] = fun { xs: Arr[Int] =>
      xs.map { x: Exp[Int] => x + 1 }
    }
    val kernelStore = KernelStore.open(s, prefix)

    it("Lua") {
      val kernel = kernelStore.createKernel("inc", KernelType.Lua, incFun)
      val in = Array(10)
      val res = kernel(in)
      assertResult(Array(11))(res)
    }

    it("LMS") {
      val kernel = kernelStore.createKernel("inc", KernelType.Scala, incFun)
      val in = Array(10)
      val res = kernel(in)
      assertResult(Array(11))(res)
    }
  }

//  case class TestCtx[SCake <: ScalanDslExp](s: SCake) {
//    val compiler = new SqliteLuaCompiler[s.type](s, true)
//    compiler.codegen.emitImports = false
//    val bridge = new ScalanSqlBridge[s.type](TPCH.Schema, s)
//    import compiler.scalan._
//    val kstore = SqliteKernelStore(compiler, prefix)
//  }
//
//  test("Q1") {
//    val s = new ScalanDslExp {}
//    val ctx = TestCtx(s)
//    import ctx.compiler.scalan._
//    val q1 = ctx.bridge.sqlQueryExp(TPCH.Q1.sql)
////    q1.show()
//    val q1kernel = ctx.kstore.createKernel("q1", q1)
//  }
//
//  test("sum") {
//    val s = new SqliteScalanExp {}
//    val ctx = TestCtx(s)
//    import ctx.compiler.scalan._
//    val sum = ctx.bridge.sqlQueryExp(
//      """
//        | select ps_partkey, ps_suppkey, avg(ps_availqty)
//        | from partsupp
//        | where ps_supplycost <= 100.0
//        | group by ps_partkey, ps_suppkey
//      """.stripMargin)
////    sum.show()
//    val sumkernel = ctx.kstore.createKernel("sum", sum)
//  }

}
