package scalan.compilation

import scalan.{ScalanDslExp, BaseTests}

class SqliteKernelTests extends BaseTests {

  ignore("t1") {
    val s = new ScalanDslExp {}
    val kstore = KernelStore.open(s, KernelTypes.ScalaKernel)
    import kstore.scalan._
    val incFun: Rep[Array[Int] => Array[Int]] = fun { xs: Arr[Int] =>
      xs.map { x: Exp[Int] => x + 1 }
    }
    val incK = kstore.createKernel("inc", incFun)
    val in = Array(10)
    val res = incK(in)
    assertResult(Array(11))(res)
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
