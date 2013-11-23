/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package makro

import scala.util.parsing.combinator.JavaTokenParsers

trait ScalanAst {

  // Tpe universe --------------------------------------------------------------------------
  abstract class TpeExpr
  type TpeExprs = List[TpeExpr]
  case class TraitCall(name: String, tpeExprs: List[TpeExpr] = Nil) extends TpeExpr
  case object TpeInt extends TpeExpr
  case object TpeBoolean extends TpeExpr
  case object TpeFloat extends TpeExpr
  case object TpeString extends TpeExpr
  case class TpeTuple(items: List[TpeExpr]) extends TpeExpr

  // Expr universe --------------------------------------------------------------------------
  abstract class Expr
  case class MethodCall(obj: Expr, name: String, args: List[Expr] = Nil) extends Expr

  // BodyItem universe ----------------------------------------------------------------------
  abstract class BodyItem
  case class MethodDef(name: String, tpeArgs: TpeArgs = Nil, args: List[MethodArg] = Nil, tpeRes: TpeExpr) extends BodyItem

  case class TpeArg(name: String, bound: Option[TpeExpr] = None)
  type TpeArgs = List[TpeArg]

  case class MethodArg(name: String, tpe: TpeExpr, default: Option[Expr] = None)
  type MethodArgs = List[MethodArg]

  case class TraitDef(
    name: String,
    tpeArgs: List[TpeArg] = Nil,
    ancestors: List[TraitCall] = Nil,
    body: List[BodyItem] = Nil)

}

trait ScalanParsers extends JavaTokenParsers  { self: ScalanAst =>

  lazy val tpeArg: Parser[TpeArg] = (ident ~ opt("<:" ~ tpeExpr)) ^^ {
    case name ~ None => TpeArg(name)
    case name ~ Some(_ ~ bound) => TpeArg(name, Some(bound))
  }

  lazy val tpeArgs = "[" ~ tpeArg ~ rep( ("," ~ tpeArg) ^^ { case _ ~ x => x }) ~ "]" ^^ {
    case _ ~ x ~ xs ~ _ => x :: xs
  }

  lazy val extendsList = traitCall ~ rep( ("with" ~ traitCall) ^^ { case _ ~ x => x }) ^^ {
    case x ~ xs => x :: xs
  }

  lazy val tpeBase: Parser[TpeExpr] = "Int" ^^^ { TpeInt } |
                     "Boolean" ^^^ { TpeBoolean } |
                     "Float" ^^^ { TpeFloat } |
                     "String" ^^^ { TpeString }

  lazy val tpeTuple = "(" ~ tpeExpr ~ rep(("," ~ tpeExpr) ^^ { case _ ~ x => x }) ~ ")" ^^ {
    case _ ~ x ~ xs ~ _ => TpeTuple(x :: xs)
  }

  lazy val tpeExprs = "[" ~ tpeExpr ~ rep(("," ~ tpeExpr) ^^ { case _ ~ x => x }) ~ "]" ^^ {
    case _ ~ x ~ xs ~ _ => x :: xs
  }

  lazy val traitCall = ident ~ opt(tpeExprs) ^^ {
    case n ~ None => TraitCall(n)
    case n ~ Some(ts) => TraitCall(n, ts)
  }

  lazy val tpeExpr: Parser[TpeExpr] =
    tpeBase |
    tpeTuple  |
    traitCall

  lazy val methodArg = ident ~ ":" ~ tpeExpr ^^ { case n ~ _ ~ t => MethodArg(n, t, None)}

  lazy val methodArgs = "(" ~ methodArg ~ rep("," ~ methodArg ^^ { _._2 }) ~ ")" ^^ {
    case _ ~ x ~ xs ~ _ => x :: xs
  }

  lazy val methodDef = "def" ~ ident ~ opt(tpeArgs) ~ opt(methodArgs) ~ ":" ~ tpeExpr ^^ {
    case _ ~ n ~ targs ~ args ~ _ ~ tres =>
      MethodDef(n, targs.toList.flatten, args.toList.flatten, tres)
  }

  lazy val bodyItem = methodDef

  lazy val bodyItems = rep(bodyItem)

  lazy val traitBody = "{" ~ opt(bodyItems) ~ "}" ^^ { case _ ~ body ~ _ => body.toList.flatten }

  lazy val traitDef = "trait" ~ ident ~ opt(tpeArgs) ~ opt(extendsList) ~ opt(traitBody) ^^ {
    case _ ~ n ~ targs ~ ancs ~ body =>
      TraitDef(n, targs.toList.flatten, ancs.toList.flatten, body.toList.flatten)
  }

  def parseTrait(s: String) = parseAll(traitDef, s).get
  def parseTpeExpr(s: String) = parseAll(tpeExpr, s).get
  def parseTpeArgs(s: String) = parseAll(tpeArgs, s).get

}

object ScalanImpl
  extends ScalanParsers
     with ScalanAst
{
}

