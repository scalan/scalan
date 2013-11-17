package scalan.codegen.emit.ast

/**
 * Used in the list of functions parameters
 * t name
 */
abstract class ArgDecl(val t: Type, val name: String) extends AST

/**
 * void fooBar(t name)
 */
case class FuncArg(
    override val t: Type,
    override val name: String
) extends ArgDecl(t, name)

/**
 * Definition of a function
 * [static] returnType name(args) body
 */
case class FuncDecl(
  templateArgs: List[Type],
	returnType: Type,
	name: String,
	args: List[ArgDecl],
	body: CompoundStat,
  isStatic: Boolean = false) extends AST

/**
 * Definition of a class
 * template <types> class name: parent { body }
 */

case class ClassDecl(
                     tyArgs: List[Type],
                     name: String,
                     typedefs : List[TypeDef],
                     //parent: Type,
                     initMembers: (List[VarDeclInit]),
                     members: (List[VarDecl], List[VarDecl]),
                     constructors: List[FuncDecl],
                     body: (List[FuncDecl], List[FuncDecl])
                     ) extends AST
