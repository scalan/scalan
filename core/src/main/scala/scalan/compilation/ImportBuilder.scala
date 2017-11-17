package scalan.compilation

import scala.collection.mutable
import scalan.meta.{SName, ImportItem}

class ImportBuilder {
  /** Items imported from each package, constructed lazily based on actual usage */
  val importedItems = mutable.Map[String, ImportItem]()

  def findImportItem(n: SName) =
    importedItems.get(n.packageName).filter { item => n.isImportedBy(item)}

  def addImport(n: SName): Boolean = {
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

