package scalan.codegen.emit.ast

abstract class Stat extends AST

/**
 *	{
 * 		// stats
 * 	}
 */
case class CompoundStat(
	stats: List[Stat]
) extends Stat

object EmptyStat extends CompoundStat(Nil)

case class ReturnStat(
    expr: Expr
) extends Stat

abstract class SelectionStat(
	cond: Expr
) extends Stat

object BreakStat extends Stat

/**
 *	if (Expr) {
 *		// body1
 *	} else {
 *		// body2
 *  }
 */
case class IfStat(
	cond: Expr,
	ts: CompoundStat,
  es: CompoundStat
) extends SelectionStat(cond)



abstract class IterStat(
    body: CompoundStat
) extends Stat

case class ForLoop(
    init: Stat,
    condition: Expr,
    step: Stat,
    body: CompoundStat
) extends IterStat(body)

case class WhileLoop(
  condition : Expr,
  step: Stat, invertCond: Boolean = false) extends Stat

object InfLoop {
  def apply(body: CompoundStat) = new ForLoop(EmptyExpr, EmptyExpr, EmptyExpr, body)

  def unapply(iter: IterStat): Option[ForLoop] = iter match {
    case l@ForLoop(EmptyStat, EmptyExpr, EmptyStat, body) => Some(l)
    case _ => None
  }
}

/**
 * 	for (;;) {
 * 		// body
 *	}
 */
//TODO remove (it can be expressed using ForLoop, EmptyStat and helper methods)
//case class InfLoop(
//	body: CompoundStat
//) extends IterStat(body)

// t v
abstract class DeclStat(
    t: Type,
    v: Var
) extends Stat {
  def isStatic = false
}

// pragma text
case class PragmaDecl(
    text: String
) extends Stat
// t v
case class VarDecl(
    t: Type,
    v: Var,
    override val isStatic: Boolean = false
) extends DeclStat(t, v)

// t v = expr
case class VarDeclInit(
    t: Type,
    v: Var,
    expr: Expr,
    override val isStatic: Boolean = false
) extends DeclStat(t, v)

// t v = expr
case class SparkVarDeclInit(
    t: Type,
    v: Var,
    expr: Expr
) extends DeclStat(t, v)

// t v(params)
case class VarDeclConstr(
    t: Type,
    v: Var,
    params: List[Expr]
) extends DeclStat(t, v)
