package scalan.compilation.lms.graph

import scala.virtualization.lms.common._
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.{LmsBackendFacade, BaseCodegen}
import java.io.{File, PrintWriter}

import scalan.util.{StringUtil, FileUtil}


/**
 * GraphCodegen is variable used in in LmsBackend-based classes as addition codegen
 * created by Adel
 */

class GraphCodegen[BackendCake <: LmsBackendFacade](backend: BackendCake) extends BaseCodegen[BackendCake]
  with LoopFusionOpt
  with BaseGenIfThenElseFat
  with BaseGenArrayBuilderOps
  with BaseGenLoopsFat
{
  override val IR: BackendCake = backend
  import IR._

  //override def codeExtension: String = "_dot_"


  case class AssignDef[+A:Manifest](id:Int, rhs: Option[Def[_]], withDeclare:Boolean, withMonoid:String = "=") extends Exp[A]  //id - for simplify, evidence$1 - Manifest for extract type
  case class AssignExp[+A:Manifest](id:Int, rhs: Option[Exp[A]], withDeclare:Boolean, withMonoid:String = "=") extends Exp[A]  //id - for simplify, evidence$1 - Manifest for extract type

  case class BlockExp(block: ControlGraphBlock) extends Exp[Any]

  class ControlGraphBlock (val results: List[Sym[Any]])  { self =>
    var statements: List[Exp[_]] = Nil
    def addSubBlockToStatements(block: ControlGraphBlock) { statements ::=  BlockExp(block)}
  }

  case class IfBlock(condition: Exp[Boolean], r: List[Sym[Any]]) extends ControlGraphBlock(r){
    var trueBlock: Option[ControlGraphBlock] = None
    var falseBlock: Option[ControlGraphBlock] = None
    override def addSubBlockToStatements(block: ControlGraphBlock) {}
  }

  case class WhileBlock (symC: Sym[Any]) extends ControlGraphBlock(Nil){  //Sym[Boolean]???
    var condBlock: ControlGraphBlock = new ControlGraphBlock(Nil)
    var block: ControlGraphBlock = new ControlGraphBlock(Nil)
    override def addSubBlockToStatements(block: ControlGraphBlock) {}
  }

  case class SimpleLoopBlock (size: Exp[Int], val v: Sym[Int]) extends ControlGraphBlock(Nil) {
  }

  case class FuncBlock(args: List[Sym[_]], name: String, r: List[Sym[Any]]) extends ControlGraphBlock(r){
  }

  // ===================================================
  // LmsOutputGraph
  //
  class LmsOutputGraph{
    var blocksStack: List[ControlGraphBlock] = Nil

    def blockBegin(block:ControlGraphBlock) {
      blocksStack.isEmpty match
      {
        case true => roots ::= block   //add first-level block to roots list
        case false => blocksStack.head.addSubBlockToStatements(block) //add block into list of statements of upper block
      }
      blocksStack ::= block
    }

    def blockEnd() {
      blocksStack = blocksStack.tail
    }


    var roots: List[ControlGraphBlock] = Nil

    def addAssignDef(v: AssignDef[_]): Unit = {
      blocksStack.head.statements ::= v
    }

    def addAssignExp(v: AssignExp[_]): Unit = {
      blocksStack.head.statements ::= v
    }

    def clean() {roots = Nil; blocksStack = Nil}


    /*
     *  export to graphViz - not compilled in separate class
     */

    def exportToGraphVis(file: File, config:GraphVizConfig) {
      if (!config.emitGraphs)
        return


      FileUtil.withFile(file) { stream =>

        stream.println( s"""digraph "${file.getName}" {""")

        stream.println("compound=true concentrate=true " + config.orientationString)

        edgesCollector.clear()

        for(root <- roots)
          exportBlock(root)(stream, config)

        edgesCollector.map(str => stream.println(str))

        stream.println("}")

      }
    }

    val mapSym2Name = new scala.collection.mutable.HashMap[Int, String]
    val edgesCollector = new scala.collection.mutable.Queue[String]

    private def exportBlock(block: ControlGraphBlock)(implicit stream: PrintWriter, config: GraphVizConfig) {
      block match {
        case block: FuncBlock =>
          val name = block.name
          stream.println(s"$name [${name} shape=doubleoctagon]")
          exportBlock(block, name)
        case _ =>
          stream.println("\"root-block should has FuncBlock type\"")
          throw new RuntimeException ("root-block should has FuncBlock type")
      }
    }

    private def exportBlock(block: ControlGraphBlock, prevNodeName:String)(implicit stream: PrintWriter, config: GraphVizConfig):String = {

      val name = block match {
        case block: FuncBlock =>
          stream.println(s"subgraph cluster_${block.name} {")
          stream.println("style=dashed; color=\"#FFCCFF\"")  //as lambda

          for (arg <- block.args)
            exportNode(s"${block.name}", arg, "ellipse")

          val statements = block.statements.reverse
          val last = statements.foldLeft(prevNodeName:String)((prevName, node) => exportNode(s"${block.name}", node, "box", prevName))

          block.name // or last

        case block: SimpleLoopBlock =>
          val name = s"loop${block.v.id}"
          stream.println(s"subgraph cluster_${name} {")
          stream.println("style=dashed; color=\"#FFCCCC\"")  //as Thunk
          val rangeExp = AssignExp(block.v.id, Some(block.size), true, "<-")
          val coreName = exportNode(name, rangeExp, "hexagon")  // , prevNodeName
          val statements = block.statements.reverse
          val last = statements.foldLeft(coreName:String)((prevName, node) => exportNode(name, node, "box", prevName))
          coreName

        case block: IfBlock =>
          val name = block.condition match {
            case x: Sym[_]=> s"if${x.id}"
            case _ => val sym = fresh; s"if${sym.id}"
          }

          stream.println(s"subgraph cluster_${name} {")
          stream.println("style=invis; color=\"lightgray\"")
          val diamondName = exportNode(name, block.condition, "diamond")
          exportBranch(name, "true", diamondName, block.trueBlock.get)
          exportBranch(name, "false", diamondName, block.falseBlock.get)

          name
      }
      stream.println("}")
      name
    }

    private def exportBranch(superName:String, name:String, diamondName:String, block: ControlGraphBlock)(implicit stream: PrintWriter, config: GraphVizConfig): Unit = {
      stream.println(s"subgraph cluster_${superName}_$name {")
      stream.println("style=none; color=\"#FFCCCC\"")

      stream.println(s"  label=$name")
      val names = for (stm <- block.statements.reverse) yield
        exportNode(s"${superName}_$name", stm, "box")

      stream.println("}")

      stream.println(s"${diamondName} -> ${names.head} [lhead=cluster_${superName}_$name ];")
    }

    private def exportNode[T](blockName: String, node: Exp[T], shape:String, prevNodeName:String)(implicit stream: PrintWriter, config: GraphVizConfig): String = {
      def printControlEdge(x:String, y:String) = {
        edgesCollector.enqueue(s"$x -> $y " + "[weight=2 color=\"#CCCCCC\" width=5 arrowhead=obox label=\" \"]")
        y
      }
      val name = node match {
        case x: BlockExp => exportBlock(x.block, prevNodeName)
        case _ => exportNode[T](blockName, node, shape)
      }
      printControlEdge(prevNodeName, name)
      name
    }

    private def exportNode[T](blockName: String, node: Exp[T], shape:String)(implicit stream: PrintWriter, config: GraphVizConfig): String = {
      def printDependenceEdge(x:String, y:String) = {
        edgesCollector.enqueue(s"$x -> $y " + "[weight=1 color = \"#333333\"]")
        y
      }

      node match {
        case x: Sym[_] =>
          val name = s"${blockName}__x${x.id}"
          stream.println(s"${name} [label = x${x.id} shape=${shape}]")
          mapSym2Name.get(x.id) match {
            case Some(prevName) => printDependenceEdge(prevName, name)
            case None => mapSym2Name.put(x.id, name)
          }
          name
        case x: AssignDef[_] =>
          val name = s"${blockName}__x${x.id}"
          val label = config.nodeLabel(Seq(s"x${x.id}", x.withMonoid, x.rhs.getOrElse("None").toString))
          stream.println(s"$name [${label} shape=${shape}]")
          x.withDeclare match {
            case false => printDependenceEdge (mapSym2Name.getOrElse (x.id, s"${x.id}"), name)
            case true =>
          }
          mapSym2Name.put(x.id, name)
          for (sym <- syms(x.rhs))
            printDependenceEdge(mapSym2Name.getOrElse(sym.id, s"x${sym.id}"), name)
          name
        case x: AssignExp[_] =>
          val name = s"${blockName}__x${x.id}"
          val label = config.nodeLabel(Seq(s"x${x.id}", x.withMonoid, x.rhs.getOrElse("None").toString))
          stream.println(s"$name [${label} shape=${shape}]")
          x.withDeclare match {
            case false => printDependenceEdge (mapSym2Name.getOrElse (x.id, s"${x.id}"), name)
            case true =>
          }
          mapSym2Name.put(x.id, name)
          for (sym <- syms(x.rhs) )
            printDependenceEdge(mapSym2Name.getOrElse(sym.id, s"x${sym.id}"), name)
          name
        case x =>
          stream.println(StringUtil.quote(s"exp = ${x.toString}"))
          s"${blockName}__exp = ${x.toString}"
        //stream.println(StringUtil.quote(x.toString))
      }
    }

  } // class LmsOutputGraph

  // ===================================================


  var graphStream: LmsOutputGraph = new LmsOutputGraph

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, aStream: PrintWriter): List[(IR.type#Sym[Any], Any)] = {
    stream = aStream
    graphStream.clean()
    graphStream.blockBegin(FuncBlock(args, className, Nil)) //todo last arg should be result-type
    emitBlock(body)
    graphStream.blockEnd()
    Nil
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    //stream.println(s"emitNode for symbol ${sym.id} with rhs = ${rhs.toString}")
    rhs match {
      case While(c,b) =>
        val whileBlock = WhileBlock(sym)
        graphStream.blockBegin(whileBlock)

        val condBlock = new ControlGraphBlock(Nil)
        graphStream.blockBegin(condBlock)

        emitBlock(c)
        emitAssignmentRaw(sym, getBlockResult(c))  //do something with it

        graphStream.blockEnd()

        val block = new ControlGraphBlock(Nil)
        graphStream.blockBegin(block)
        emitBlock(b)
        graphStream.blockEnd()

        whileBlock.condBlock = condBlock
        whileBlock.block = block

        graphStream.blockEnd()
      case SimpleLoop(size, v, body) =>
        super.emitNode(sym, rhs)
      case _ =>
        //graphStream.addNode(sym, rhs)
        graphStream.addAssignDef(AssignDef(sym.id, Some(rhs), withDeclare = true))
    }
  }

  override def emitFatBlock(rhs: List[Block[Any]]): Unit = {
    val results = rhs.map(getBlockResultFull)
    val exp: Exp[Any] = Combine(results)
    val block = Block(exp)
    emitBlock(block) // TODO: find another way
  }

  // FROM GenericFatCodegen
  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef): Unit = {
    rhs match {
      case SimpleFatIfThenElse(c, as, bs) =>
        stream.println(s"emitFatNode for symbol ${symList.head.id}... (sz = ${symList.length}), rhs = ${rhs.toString}")
        //declare variables
        val ifBlock = IfBlock(c, symList)
        graphStream.blockBegin(ifBlock)
        
        symList.foreach({ sym => emitVarDecl(sym) })
        
        val trueBlock = new ControlGraphBlock(symList)
        graphStream.blockBegin(trueBlock)
        
        emitFatBlock(as)
        (symList zip as.map(getBlockResult)).foreach({ p => emitAssignmentRaw(p._1, p._2) })

        graphStream.blockEnd()

        //stream.println("} else {")
        val falseBlock = new ControlGraphBlock(symList)
        graphStream.blockBegin(falseBlock)

        emitFatBlock(bs)
        (symList zip bs.map(getBlockResult)).foreach({ p => emitAssignmentRaw(p._1, p._2) })
        graphStream.blockEnd()

        //stream.println("}")
        ifBlock.trueBlock = Some(trueBlock)
        ifBlock.falseBlock = Some(falseBlock)
        graphStream.blockEnd()

      case SimpleFatLoop(size, v, body) =>
        for ((l,r) <- symList zip body) {
          r match {
            case ArrayElem(y) =>
              emitNode(l, ArrayNew(size))
            case ReduceElem(y) =>
              emitNode(l, NewVar(Const(0.0)))
            case ArrayIfElem(c,y) =>
              emitNode(l, ArrayNew(Const(0)))
            case ReduceIfElem(c,y) =>
              emitNode(l, NewVar(Const(0.0)))
            case ReduceIfIntElem(c,y) =>
              emitNode(l, NewVar(Const(0)))
            case x =>
              stream.println(s"ERROR: SimpleFatLoop: can not generate init var for {${l} = ${r}}")
          }
        }

        graphStream.blockBegin(SimpleLoopBlock(size, v))
        emitFatBlock(syms(body).map(Block(_)))

        for ((l,r) <- symList zip body) {
          r match {
            case ArrayElem(y) =>
              emitAssignmentRaw(l, getBlockResult(y))
            case ReduceElem(y) =>
              emitAssignmentRawWithMonoid(l, getBlockResult(y), "+=")
            /* todo think about combine 'if' and 'monoid'
              SimpleFatIfThenElse(c, as, bs)
            case ArrayIfElem(c,y) =>
              emitAssignmentRaw(l, getBlockResult(y))
            case ReduceIfElem(c,y) =>
              emitAssignmentRawWithMonoid(l, getBlockResult(y), "+=")
              */
            case x =>
              stream.println(s"ERROR: SimpleFatLoop: can not generate update for var for {${l} = ${r}}")
          }
        }

        graphStream.blockEnd()

      case _ =>
        stream.println(s"NOT emitFatNode for symbol ${symList.head.id}, because  rhs not mached to SimpleFatIfThenElse")
        super.emitFatNode(symList, rhs)
    }

  }

  def emitAssignmentRaw(sym: Sym[Any], rhs: Def[Any]): Unit =
    graphStream.addAssignDef(AssignDef(sym.id, Some(rhs), withDeclare = false))

  def emitAssignmentRaw(sym: Sym[Any], rhs: Exp[Any]): Unit =
    graphStream.addAssignExp(AssignExp(sym.id, Some(rhs), withDeclare = false))

  def emitAssignmentRawWithMonoid(sym: Sym[Any], rhs: Def[Any], withMonoid:String): Unit =
    graphStream.addAssignDef(AssignDef(sym.id, Some(rhs), withDeclare = false, withMonoid))

  def emitAssignmentRawWithMonoid(sym: Sym[Any], rhs: Exp[Any], withMonoid:String): Unit =
    graphStream.addAssignExp(AssignExp(sym.id, Some(rhs), withDeclare = false, withMonoid))


  override def emitVarDecl(sym: Sym[Any]) = {
    graphStream.addAssignExp(AssignExp(sym.id, Some(sym), withDeclare = true))
  }


  //copy-pasted from scala.virtualization.lms.epfl.test7.ScalaGenFatArrayLoopsFusionOpt - important for fuse can worked
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Block(a)) => Some(a) //TODO: block??
    case _ => super.unapplySimpleCollect(e)
  }

  /*override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,Block(a)) => Some((a,List(c))) //TODO: block?
    case _ => super.unapplySimpleCollectIf(e)
  } */

  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match { //TODO: should c be list or not?
    case ArrayElem(a) if c.length == 1 => ArrayIfElem(c(0),a)
    case ReduceElem(a) if c.length == 1 => ReduceIfElem(c(0),a)
    case ReduceIntElem(a) if c.length == 1 => ReduceIfIntElem(c(0),a)
    case _ => super.applyAddCondition(e,c)
  }
  //copy-pasting end



  override def emitValDef(sym: Sym[Any], rhs: String) = {
    stream.println(s"ERROR: emitValDef ${sym.id} = ${rhs} should be caught before convert rhs to string")
  }

  override def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    stream.println(s"ERROR: emitAssignment ${sym.id} = $rhs should be caught before convert rhs to string")
  }

}


/*
trait NotGraphCodegenForJumpToItsMixins extends LmsBackend
  with CxxShptrCodegen
  with CLikeGenEqual //not important
  with CLikeGenPrimitiveOps //not important
  with CxxShptrGenStruct
  with CxxShptrGenFatArrayLoopsFusionOpt
  with CxxShptrGenCastingOps
  with CxxShptrGenIfThenElseFat
  with CLikeGenOrderingOps ////-not important
  with CLikeGenBooleanOps////-not important
  with CxxShptrGenFunctions
  with CxxShptrGenArrayOpsBoost
  with CxxShptrGenVariables
  with CxxShptrGenArrayBuilderOps
  with CxxShptrGenRangeOps
  with CLikeGenWhile // may be not important
  with CLikeGenNumericOps ////-not important
  with CxxShptrGenListOps
  with CxxShptrGenLstOps
  //with CxxShptrGenJNIExtractor
{ self => }
  */
