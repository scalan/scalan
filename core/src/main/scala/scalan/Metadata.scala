package scalan

trait Metadata { self: Scalan =>
  // exists only to avoid duplicate keys
  private[this] var metaKeyNames = Set.empty[String]

  // all metadata is currently global, may add scoping in the future
  // Elem[A] ensures it's a known serializable type so that external tools can
  // handle metadata. We may weaken this restriction later
  /**
   * Key for metadata of type `A`.
   */
  case class MetaKey[A](name: String)(implicit e: Elem[A]) {
    if (name == null)
      !!!(s"Metadata key is null")
    else if (metaKeyNames.contains(name)) {
      !!!(s"Duplicate metadata key: $name")
    } else {
      metaKeyNames += name
    }
  }

  /**
   * Sets metadata for the target. No-op in sequential context,
   * but the metadata can be accessed, transformed, etc. in staged context.
   *
   * Returns the target for chaining.
   */
  def setMetadata[A, B](target: Rep[A], key: MetaKey[B])(value: B): Rep[A]

  implicit class MetadataOps[A](target: Rep[A]) {
    def setMetadata[B](key: MetaKey[B])(value: B) = self.setMetadata(target, key)(value)
  }
}

trait MetadataSeq extends Metadata { self: ScalanSeq =>
  def setMetadata[A, B](target: Rep[A], key: MetaKey[B])(value: B): Rep[A] = target
}

trait MetadataExp extends Metadata { self: ScalanExp =>
  case class MetaNode(private val meta: Map[MetaKey[_], Any]) {
    def get[A](key: MetaKey[A]) = meta.get(key).asInstanceOf[Option[A]]

    def set[A](key: MetaKey[A])(value: A) = new MetaNode(meta.updated(key, value))

    def remove[A](key: MetaKey[A]) = new MetaNode(meta - key)

    def extendWith(other: MetaNode) = new MetaNode(meta ++ other.meta.toSeq)

    def updateIfExists[A](key: MetaKey[A])(f: A => A) = get(key) match {
      case None => this
      case Some(value) => set(key)(f(value))
    }

    def update[A](key: MetaKey[A], default: A)(f: A => A) = get(key) match {
      case None => set(key)(default)
      case Some(value) => set(key)(f(value))
    }
  }

  object MetaNode {
    val empty = new MetaNode(Map.empty)
  }

  private var metadataPool = Map.empty[Exp[_], MetaNode]

  def allMetadataOf(target: Rep[_]): MetaNode = metadataPool.getOrElse(target, MetaNode.empty)

  protected[scalan] def setAllMetadata(target: Rep[_], node: MetaNode) = {
    val newNode = metadataPool.get(target) match {
      case None => node
      case Some(oldNode) => oldNode.extendWith(node)
    }
    metadataPool += target -> newNode
  }

  def setMetadata[A, B](target: Rep[A], key: MetaKey[B])(value: B): Rep[A] = {
    val node = allMetadataOf(target)
    metadataPool += target -> node.set(key)(value)
    target
  }

  def getMetadata[A](target: Rep[_], key: MetaKey[A]): Option[A] =
    allMetadataOf(target).get(key)

  implicit class MetadataOpsExp(target: Rep[_]) {
    def getMetadata[A](key: MetaKey[A]) = self.getMetadata(target, key)

    def allMetadata = self.allMetadataOf(target)
  }
}