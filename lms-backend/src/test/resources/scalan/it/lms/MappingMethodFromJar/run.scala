package scalan.it.lms.MappingMethodFromJar

object run {

  def main(args: Array[String]): Unit = {
   println(execute(new Exception("xxx")))
  }

  def execute(s: Throwable): String = {
    val m = new MappingMethodFromJar()
    m(s)
  }
}