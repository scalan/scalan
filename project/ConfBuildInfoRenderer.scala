import sbtbuildinfo._

case class ConfBuildInfoRenderer(options: Seq[BuildInfoOption]) extends BuildInfoRenderer {
  def fileType = BuildInfoType.Resource
  def extension = "conf"
  def header = Nil
  def footer = Nil

  // TODO any other needed types
  def render(result: BuildInfoResult) = {
    val rhs = result.typeExpr match {
      case TypeExpression("String", Nil) =>
        "\"" + result.value.toString + "\""
      case _ =>
        result.value.toString
    }
    s"""${result.identifier}=$rhs"""
  }

  def renderKeys(infoKeysNameAndValues: Seq[BuildInfoResult]): Seq[String] = infoKeysNameAndValues.map(render)
}
