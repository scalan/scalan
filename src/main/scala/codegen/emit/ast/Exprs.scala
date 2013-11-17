package scalan.codegen.emit.ast

abstract class Expr extends Stat {
  def method(name: String, types: List[Type], arg1: Expr, args: Expr*): MethodCall = method(name, types, arg1 :: args.toList)
  def method(name: String, types: List[Type], args: List[Expr]) = MethodCall(this, name, types, args)
  def field(name: String) = ClassField(this, name)
  def call(types: List[Type], args: Expr*) = FuncCall(this, types, args.toList)
  def assign(rhs: Expr) = BinExpr(this, rhs, Operator.assignOp)
  def index(i: Expr) = ArrIndex(this, i)
}

object EmptyExpr extends Expr

/**
 * (Expr)
 */
case class ParenExpr(
    expr: Expr
) extends Expr

/**
 * expr1 op expr2
 */
case class BinExpr(
	expr1: Expr,
	expr2: Expr,
	op: Operator
) extends Expr

case class UnaryExpr(op: Operator,
                     expr: Expr,
                     isPostfix: Boolean = false) extends Expr

case class TypeCast(t: Type, expr: Expr) extends Expr
case class TypeDef(t: Type, name : String) extends Expr

abstract class Const extends Expr
case class IntConst(value: Int) extends Const
case class FloatConst(value: Float) extends Const
case class BooleanConst(value: Boolean) extends Const

abstract class Ptr extends Expr

/**
* new t[expr]
*/
case class NewArray(
    t: Type,
    expr: Expr
) extends Ptr

/**
 * name
 */
case class Var(name: String) extends Expr

/**
 * classInst.fieldName;
 */
case class ClassField(
    classInst: Expr,
    fieldName: String
) extends Expr


/**
 * (func)(params)
 */
case class FuncCall(
    func: Expr,
    types: List[Type],
    params: List[Expr]
) extends Expr

/**
 * classInst.methodName(params)
 */
case class MethodCall(
    classInst: Expr,
    methodName: String,
    types: List[Type],
    params: List[Expr]
) extends Expr

/**
 * classInst.methodName(params)
 */
case class StaticMethodCall(
    classType: Type,
    methodName: String,
    params: List[Expr]
) extends Expr

case class ArrIndex(arrInst: Expr, index: Expr) extends Expr