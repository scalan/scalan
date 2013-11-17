package scalan.codegen.emit.ast


trait AST

/**
 * Provides data types.
 */
abstract class Type extends AST {
  def ptr: Type = PtrType(this)
  def const: Type = ConstType(this)
  def constexpr: Type = ConstexprType(this)
  def ref: Type = RefType(this)
  def toPA: Type = PAType(this)
  def toNA: Type = NAType(this)
  def toChunks: Type = ChunksType(this)
  def toPAChunks: Type = PAChunksType(this)
  def staticMethod(name: String, args: List[Expr]) = StaticMethodCall(this, name, args)
}

// int, size_t, ...
case class SimpleType(name: String) extends Type

// class "name". Need for template functions
case class ClassType(name: String) extends Type

// t&
case class RefType(t: Type) extends Type

// const t
case class ConstType(t: Type) extends Type

// constexpr t
case class ConstexprType(t: Type) extends Type

// t*
case class PtrType(t: Type) extends Type
case class PAType(t: Type) extends Type
case class NAType(t: Type) extends Type
case class PairType(t1: Type, t2: Type) extends Type
case class PairArrayType(t1: Type, t2: Type) extends Type

case class ChunksType(t: Type) extends Type
case class PAChunksType(t: Type) extends Type
case class PairChunksType(t1: Type, t2: Type) extends Type


// name<arg0,...,argN>
case class TemplateType(name: String, args: List[Type]) extends Type

object Type {
  import Types._
  def apply(name: String) = name match {
    case "Int" => IntType
    case "Float" => FloatType
    case "Boolean" => BooleanType
    case "Double" => DoubleType
    case _ => !!!("unknown type name: %s".format(name), name)
  }
}

object Types {
  val UnitType = SimpleType("Unit")
  val UnitArrayType = SimpleType("UnitArray")
  val IntType = SimpleType("Int")
  val FloatType = SimpleType("Float")
  val DoubleType = SimpleType("Float")
  val BooleanType = SimpleType("Boolean")
  val StringType = SimpleType("char*")
  val SizeType = SimpleType("size_t")
  val VoidType = SimpleType("void")
  val LongType = SimpleType("long")
  val AutoType = SimpleType("auto")
}
