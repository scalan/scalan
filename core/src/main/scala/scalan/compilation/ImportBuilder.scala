package scalan.compilation

import scala.collection.mutable

case class Name(packageName: String, name: String) {
  def isImportedBy(item: ImportItem): Boolean = {
    if (packageName != item.packageName) return false
    item.importedNames.contains(Name.ImportAllWildcard) || item.importedNames.contains(name)
  }
}
object Name {
  /** Wildcard character used to signify imporing all names from namespace */
  val ImportAllWildcard = "*"

}

case class ImportItem(packageName: String, importedNames: List[String])

class ImportBuilder {
  /** Items imported from each package, constructed lazily based on actual usage */
  val importedItems = mutable.Map[String, ImportItem]()

  def findImportItem(n: Name) =
    importedItems.get(n.packageName).filter { item => n.isImportedBy(item)}

  def addImport(n: Name): Boolean = {
    val existingImport = importedItems.find { case (p, item) => item.importedNames.contains(n.name) }
    existingImport match {
      case Some((_, ImportItem(pn, _))) if pn == n.packageName => true // already imported
      case Some(_) =>
        false // name conflict with another package
      case None =>
        // can be added
        importedItems.get(n.packageName) match {
          case Some(item) =>
            val newImportItem = item.copy(importedNames = n.name :: item.importedNames)
            importedItems(n.packageName) = newImportItem
          case None =>
            importedItems(n.packageName) = ImportItem(n.packageName, List(n.name))
        }
        true
    }
  }
}

