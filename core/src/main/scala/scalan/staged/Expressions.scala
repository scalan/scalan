package scalan.staged

import java.lang.reflect.Constructor
import java.util.{Objects, Arrays}
import com.github.kxbmap.configs.syntax.ConfigOps
import com.typesafe.config.{ConfigFactory, Config}
import com.typesafe.scalalogging.LazyLogging
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{mutable, TraversableOnce}
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.reflect.runtime.universe._
import scalan.util.{ParamMirror, ReflectionUtil}
import scalan.{Scalan, Base, Plugins}
import scalan.common.Lazy
import scalan.compilation.GraphVizConfig

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).
 *
 * @since 0.1
 */
trait Expressions extends Base { scalan: Scalan =>
  /**
   * A Sym is a symbolic reference used internally to refer to expressions.
   */
  object Sym {
    private var currId = 0
    def fresh[T: LElem]: Exp[T] = {
      currId += 1
      Sym(currId)
    }
  }
  case class Sym[+T](id: Int)(implicit private val eT: LElem[T @uncheckedVariance]) extends Exp[T] {
    override def elem: Elem[T @uncheckedVariance] = this match {
      case Def(d) => d.selfType
      case _ => eT.value
    }
    def varName = "s" + id
    override def toString = varName

    lazy val definition = findDefinition(this).map(_.rhs)
    def toStringWithDefinition = toStringWithType + definition.map(d => s" = $d").getOrElse("")
  }

  def fresh[T: LElem]: Exp[T] = Sym.fresh[T]

  case class TableEntrySingle[T](sym: Exp[T], rhs: Def[T], lambda: Option[Exp[_]]) extends TableEntry[T]

  override val TableEntry: TableEntryCompanion = new TableEntryCompanion {
    def apply[T](sym: Exp[T], rhs: Def[T]) = new TableEntrySingle(sym, rhs, None)
    def apply[T](sym: Exp[T], rhs: Def[T], lam: Exp[_]) = new TableEntrySingle(sym, rhs, Some(lam))
    def unapply[T](tp: TableEntry[T]): Option[(Exp[T], Def[T])] = Some((tp.sym, tp.rhs))
    //def unapply[T](s: Exp[T]): Option[TableEntry[T]] = findDefinition(s)
  }
  protected val globalThunkSym: Exp[_] = fresh[Int] // we could use any type here
  private[this] val expToGlobalDefs: mutable.Map[Exp[_], TableEntry[_]] = mutable.HashMap.empty
  private[this] val defToGlobalDefs: mutable.Map[(Exp[_], Def[_]), TableEntry[_]] = mutable.HashMap.empty

  def findDefinition[T](s: Exp[T]): Option[TableEntry[T]] =
    expToGlobalDefs.get(s).asInstanceOf[Option[TableEntry[T]]]

  def findDefinition[T](thunk: Exp[_], d: Def[T]): Option[TableEntry[T]] =
    defToGlobalDefs.get((thunk,d)).asInstanceOf[Option[TableEntry[T]]]

  def findOrCreateDefinition[T](d: Def[T], newSym: => Exp[T]): Exp[T] = {
    val optScope = thunkStack.top
    val optFound = optScope match {
      case Some(scope) =>
        scope.findDef(d)
      case None =>
        findDefinition(globalThunkSym, d)
    }
    val te = optFound.getOrElse {
      createDefinition(optScope, newSym, d)
    }
    assert(te.rhs == d, s"${if (optFound.isDefined) "Found" else "Created"} unequal definition ${te.rhs} with symbol ${te.sym.toStringWithType} for $d")
    te.sym

  }

  def createDefinition[T](s: Exp[T], d: Def[T]): TableEntry[T] =
    createDefinition(thunkStack.top, s, d)

  private def createDefinition[T](optScope: Option[ThunkScope], s: Exp[T], d: Def[T]): TableEntry[T] = {
    val te = lambdaStack.top match {
      case Some(fSym) => TableEntry(s, d, fSym)
      case _ => TableEntry(s, d)
    }
    optScope match {
      case Some(scope) =>
        defToGlobalDefs += (scope.thunkSym, te.rhs) -> te
        scope += te
      case None =>
        defToGlobalDefs += (globalThunkSym, te.rhs) -> te
    }

    expToGlobalDefs += te.sym -> te
    te
  }

  /**
   * Updates the universe of symbols and definitions, then rewrites until fix-point
   * @param d A new graph node to add to the universe
   * @param newSym A symbol that will be used if d doesn't exist in the universe
   * @tparam T
   * @return The symbol of the graph which is semantically(up to rewrites) equivalent to d
   */
  protected[scalan] def toExp[T](d: Def[T], newSym: => Exp[T]): Exp[T] = {
    var res = findOrCreateDefinition(d, newSym)
    var currSym = res
    var currDef = d
    do {
      currSym = res
      val ns = rewrite(currSym).asRep[T]
      ns match {
        case null =>
          currDef = null
        case Def(someOtherD) =>
          res = ns
          currDef = someOtherD
        case _ =>
          res = ns
          currDef = null
      }
    } while (res != currSym && currDef != null)
    res
  }
}

