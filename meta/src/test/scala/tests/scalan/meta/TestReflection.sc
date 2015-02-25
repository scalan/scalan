import scala.reflect.runtime.{universe => ru}
val l = List(1,2,3)
def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]
val theType = getTypeTag(l).tpe




