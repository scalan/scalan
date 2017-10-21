package scalan.meta

case class ImportItem(packageName: String, importedNames: List[String])

case class Name(packageName: String, name: String) {
  import Name._
  def mkFullName = fullNameString(packageName, name)
  def isImportedBy(item: ImportItem): Boolean = {
    if (packageName != item.packageName) return false
    item.importedNames.contains(Name.ImportAllWildcard) || item.importedNames.contains(name)
  }
}

object Name {
  /** Wildcard character used to signify imporing all names from namespace */
  val ImportAllWildcard = "*"
  def fullNameString(packageName: String, name: String): String = s"$packageName.$name"
}