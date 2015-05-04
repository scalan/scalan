package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.staged.Expressions
import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.{Utils, Lazy}

/*
 The code is taken from LMS and is used in Scalan with the same semantics
 in order to easily translate operations to the equivalents via LmsBridge.
 Their usage in Scalan is limited to be consistent with functional semantics of Scalan.
 Don't expect everything possible in LMS to be also possible in Scalan in the same way.
 There are changes in the code:
 - Sym -> Exp
 - Manifest -> Elem
 - infix -> implicit class
 - no SourceContext, withPos
 - mirroring implemented in Scalan way (though consistent with LMS)
 */

trait Effects { self: Scalan =>


}

trait EffectsSeq extends Effects { self: ScalanSeq =>
}

trait EffectsExp extends Expressions with Effects with Utils with GraphVizExport { self: ScalanExp =>

  case class Block[+T](val res: Exp[T]) { def elem: Elem[T @uncheckedVariance] = res.elem } // variance ...

  def blocks(e: Any): List[Block[Any]] = e match {
    case b: Block[Any] => List(b)
    case p: Product => flatMapProduct(p, blocks(_))
    case _ => Nil
  }

  type State = List[Exp[Any]] // TODO: maybe use TableEntry instead to save lookup

  var context: State = _

  var conditionalScope = false // used to construct Control nodes

  var globalDefs: List[Stm] = Nil
  var localDefs: List[Stm] = Nil
  var globalDefsCache: Map[Exp[Any],Stm] = Map.empty

  def reifySubGraph[T](b: =>T): (T, List[Stm]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    val saveGlobalCache = globalDefsCache
    localDefs = Nil
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    globalDefsCache = saveGlobalCache
    (r, defs)
  }

  private def allDistinct[A](xs: Iterable[A]): Boolean = {
    val seen = mutable.HashSet[A]()
    xs.foreach { x => if (!seen.add(x)) { return false } }
    true
  }

  def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(allDistinct(lhs), "ERROR: multiple defs: " + ds)

    val existing = lhs flatMap (globalDefsCache get _)
    assert(existing.isEmpty, "ERROR: already defined: " + existing + " for " + ds)

    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
    for (stm <- ds; s <- stm.lhs) {
      globalDefsCache += (s -> stm)
    }
  }

  // --- class defs

  case class Reflect[+A:Elem](x: Def[A], summary: Summary, deps: List[Exp[Any]]) extends BaseDef[A] {
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = {
      val Def(mx) = x.mirror(t).asRep[A]
      reflectMirrored(Reflect(mx, mapOver(t, summary), t(deps).toList))
    }
  }

  case class Reify[A:Elem](x: Exp[A], summary: Summary, effects: List[Exp[Any]]) extends BaseDef[A] {
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = Reify(t(x), mapOver(t,summary), t(effects).toList)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case Reify(x, _, es) => s"Reify($x, [${es.mkString(",")}])"
    case Reflect(x, _, ds) => s"Reflect($x, [${ds.mkString(",")}])"
    case _ => super.formatDef(d)
  }

  def mapOver(t: Transformer, u: Summary) = { // TODO: move to effects class?
    u.copy(mayRead = t.onlySyms(u.mayRead), mstRead = t.onlySyms(u.mstRead),
      mayWrite = t.onlySyms(u.mayWrite), mstWrite = t.onlySyms(u.mstWrite))
  }

  // --- summary

  case class Summary(
    maySimple: Boolean,
    mstSimple: Boolean,
    mayGlobal: Boolean,
    mstGlobal: Boolean,
    resAlloc: Boolean,
    control: Boolean,
    mayRead: List[Exp[Any]],
    mstRead: List[Exp[Any]],
    mayWrite: List[Exp[Any]],
    mstWrite: List[Exp[Any]])

  def Pure() = new Summary(false,false,false,false,false,false,Nil,Nil,Nil,Nil)
  def Simple() = new Summary(true,true,false,false,false,false,Nil,Nil,Nil,Nil)
  def Global() = new Summary(false,false,true,true,false,false,Nil,Nil,Nil,Nil)
  def Alloc() = new Summary(false,false,false,false,true,false,Nil,Nil,Nil,Nil)
  def Control() = new Summary(false,false,false,false,false,true,Nil,Nil,Nil,Nil)

  def Read(v: List[Exp[Any]]) = { val ds = v.distinct; new Summary(false,false,false,false,false,false,ds,ds,Nil,Nil) }
  def Write(v: List[Exp[Any]]) = { val ds = v.distinct; new Summary(false,false,false,false,false,false,Nil,Nil,ds,ds) }

  def mayRead(u: Summary, as: List[Exp[Any]]): Boolean =
    u.mayGlobal || as.exists(a => u.mayRead contains a)
  def mayWrite(u: Summary, as: List[Exp[Any]]): Boolean =
    u.mayGlobal || as.exists(a => u.mayWrite contains a)
  def maySimple(u: Summary): Boolean = u.mayGlobal || u.maySimple

  def mustMutable(u: Summary): Boolean = u.resAlloc
  def mustPure(u: Summary): Boolean = u == Pure()
  def mustOnlyRead(u: Summary): Boolean = u == Pure().copy(mayRead=u.mayRead, mstRead=u.mstRead) // only reads allowed
  def mustIdempotent(u: Summary): Boolean = mustOnlyRead(u) // currently only reads are treated as idempotent

  implicit class SummaryOps(u: Summary) {
    
    def orElse(v: Summary) = new Summary(
      u.maySimple || v.maySimple, u.mstSimple && v.mstSimple,
      u.mayGlobal || v.mayGlobal, u.mstGlobal && v.mstGlobal,
      false, //u.resAlloc && v.resAlloc, <--- if/then/else will not be mutable!
      u.control || v.control,
      (u.mayRead ++ v.mayRead).distinct, (u.mstRead intersect v.mstRead),
      (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite intersect v.mstWrite)
    )

    def andAlso(v: Summary) = new Summary(
      u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
      u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
      u.resAlloc || v.resAlloc,
      u.control || v.control,
      (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
      (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
    )

    def andThen(v: Summary) = new Summary(
      u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
      u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
      v.resAlloc,
      u.control || v.control,
      (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
      (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
    )

    def star = Pure() orElse u // any number of repetitions, including 0

    def withoutControl = new Summary(
      u.maySimple, u.mstSimple,
      u.mayGlobal, u.mstGlobal,
      u.resAlloc,
      false,
      u.mayRead, u.mstRead,
      u.mayWrite, u.mstWrite
    )
  }

  def summarizeEffects(e: Block[Any]) = e match {
    case Block(Def(Reify(_,u,_))) => u
    //    case Def(Reflect(_,u,_)) => u
    case _ => Pure()
  }

  // --- reflect helpers

  def controlDep(x: Exp[Any]) = x match {
    case Def(Reflect(y,u,es)) if u == Control() => true
    case _ => false
  }

  // performance hotspot
  def nonControlSyms[R](es: List[Exp[Any]], ss: Any => List[R]): List[R] = {
    // es.filterNot(controlDep).flatMap(syms)
    val out = new scala.collection.mutable.ListBuffer[R]
    val it = es.iterator
    while (it.hasNext) {
      val e = it.next()
      if (!controlDep(e)) out ++= ss(e)
    }
    out.result()
  }

  override def syms(e: Any): List[Exp[Any]] = e match {
    case s: Summary => Nil // don't count effect summaries as dependencies!

    // enable DCE of reflect nodes if they are only control dependencies
    case Reflect(x,u,es) if addControlDeps => syms(x) ::: nonControlSyms(es, syms)
    case Reify(x,u,es) if addControlDeps => syms(x) ::: nonControlSyms(es, syms)
    case _ => super.syms(e)
  }

  override def rsyms[T](e: Any)(f: Any => List[T]): List[T] = e match { // stack overflow ...
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case _ => super.rsyms(e)(f)
  }

  override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case s: Summary => Nil // don't count effect summaries as dependencies!

    // enable DCE of reflect nodes if they are only control dependencies
    case Reflect(x,u,es) if addControlDeps => symsFreq(x) ::: nonControlSyms(es, symsFreq)
    case Reify(x,u,es) if addControlDeps => symsFreq(x) ::: nonControlSyms(es, symsFreq)
    case _ => super.symsFreq(e)
  }

  override def effectSyms(x: Any): List[Exp[Any]] = x match {
    case Def(Reify(y, u, es)) => es.asInstanceOf[List[Exp[Any]]]
    case _ => super.effectSyms(x)
  }

  def readSyms(e: Any): List[Exp[Any]] = e match {
    case Reflect(x, u, es) => readSyms(x) // ignore effect deps (they are not read!)
    case Reify(x, u, es) =>
      // in general: the result of a block is not read but passed through.
      // FIXME this piece of logic is not clear. is it a special case for unit??
      // it looks like this was introduced to prevent the Reify to be reflected
      // if x is a mutable object defined within the block.
      // TODO the globalMutableSyms part was added later (June 2012) -- make sure it does the right thing
      if ((es contains x) || (globalMutableSyms contains x)) Nil
      else readSyms(x)
    case s: Exp[_] => List(s)
    case p: Product => flatMapProduct(p, readSyms(_))
    case _ => Nil
  }

  /*
    decisions to be made:
    1) does alias imply read? or are they separate?
    2) use a data structure to track transitive aliasing or recompute always?
  */


  /*

  the methods below define the sharing relation between the
  result of an operation and its arguments.

  how do i use them? what do i need to return?

  assume an operation foo:

  y = Foo(x)

  x should be returned in the following cases:

  x in aliasSyms(y)      if y = x      // if then else
  x in containSyms(y)    if *y = x     // array update
  x in extractSyms(y)    if y = *x     // array apply
  x in copySyms(y)       if *y = *x    // array clone

  y = x is to be understood as "y may be equal to x"
  *y = x as "dereferencing y (at some index) may return x"
  etc.

  */

  def aliasSyms(e: Any): List[Exp[Any]] = e match {
    case Reflect(x, u, es) => aliasSyms(x)
    case Reify(x, u, es) => syms(x)
    case s: Exp[_] => List(s)
    case p: Product => flatMapProduct(p, aliasSyms(_))
    case _ => Nil
  }

  def containSyms(e: Any): List[Exp[Any]] = e match {
    case Reflect(x, u, es) => containSyms(x)
    case Reify(x, u, es) => Nil
    case s: Exp[_] => Nil
    case p: Product => flatMapProduct(p, containSyms(_))
    case _ => Nil
  }

  def extractSyms(e: Any): List[Exp[Any]] = e match {
    case Reflect(x, u, es) => extractSyms(x)
    case Reify(x, u, es) => Nil
    case s: Exp[_] => Nil
    case p: Product => flatMapProduct(p, extractSyms(_))
    case _ => Nil
  }

  def copySyms(e: Any): List[Exp[Any]] = e match {
    case Reflect(x, u, es) => copySyms(x)
    case Reify(x, u, es) => Nil
    case s: Exp[_] => Nil
    case p: Product => flatMapProduct(p, copySyms(_))
    case _ => Nil
  }


  def isPrimitiveType[T](e: Elem[T]) = e.isBaseType

  def noPrim(sm: List[Exp[Any]]): List[Exp[Any]] = sm.filterNot(_.elem.isBaseType)

  /*
    def allTransitiveAliases(start: Any): List[TableEntry[Any]] = {
      def deps(st: List[Exp[Any]]): List[TableEntry[Any]] = {
        val st1 = st filterNot (s => isPrimitiveType(s.tp))
        globalDefs.filter(st1 contains _.sym)
      }
      GraphUtil.stronglyConnectedComponents[TableEntry[Any]](deps(aliasSyms(start)), t => deps(aliasSyms(t.rhs))).flatten.reverse
    }
  */

  /*
   TODO: switch back to graph based formulation -- this will not work for circular deps
  */

  val shallowAliasCache = new scala.collection.mutable.HashMap[Exp[Any], List[Exp[Any]]]
  val deepAliasCache = new scala.collection.mutable.HashMap[Exp[Any], List[Exp[Any]]]
  val allAliasCache = new scala.collection.mutable.HashMap[Exp[Any], List[Exp[Any]]]

  def utilLoadStm[T](s: Exp[T]) =
    if (!isPrimitiveType(s.elem)) /*globalDefs.filter{e => e.lhs contains s}*/
      findDefinition(s).toList
    else Nil

  def utilLoadStms(s: List[Exp[Any]]) = s.flatMap(utilLoadStm)
  def utilLoadSym[T](s: Exp[T]) = utilLoadStm(s).map(_.rhs)

  def shallowAliases(start: Any): List[Exp[Any]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => a::shallowAliasCache.getOrElseUpdate(a, shallowAliases(utilLoadSym(a))) }
    val extract = noPrim(extractSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    //println("shallowAliases("+start+") = "+alias+" ++ "+extract)
    (alias ++ extract).distinct
  }

  def deepAliases(start: Any): List[Exp[Any]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val copy = noPrim(copySyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val contain = noPrim(containSyms(start)) flatMap { a => a::allAliasCache.getOrElseUpdate(a, allAliases(utilLoadSym(a))) }
    //println("aliasSyms("+start+") = "+aliasSyms(start) + "/" + noPrim(aliasSyms(start)))
    //println("copySyms("+start+") = "+copySyms(start) + "/" + noPrim(copySyms(start)))
    //println("containSyms("+start+") = "+containSyms(start) + "/" + noPrim(containSyms(start)))
    //println("deepAliases("+start+") = "+alias+" ++ "+copy+" ++ "+contain)
    (alias ++ copy ++ contain).distinct
  }


  def allAliases(start: Any): List[Exp[Any]] = {
    val r = (shallowAliases(start) ++ deepAliases(start)).distinct
    //printdbg("all aliases of " + start + ": " + r.mkString(", "))
    r
  }

  //def allTransitiveAliases(start: Any): List[Stm] = utilLoadStms(allAliases(start))
  //def transitiveAliases(start: List[Exp[Any]]): List[Stm] = start.flatMap(utilLoadSymTP)

  // TODO possible optimization: a mutable object never aliases another mutable object, so its inputs need not be followed

  def mutableTransitiveAliases(s: Any) = {
    val aliases = allAliases(s)
    val bareMutableSyms = aliases filter { o => globalMutableSyms.contains(o) }
    val definedMutableSyms = utilLoadStms(aliases) collect { case TableEntry(s2, Reflect(_, u, _)) if mustMutable(u) => s2 }
    bareMutableSyms ++ definedMutableSyms
  }


  def getActuallyReadSyms[A](d: Def[A]) = {
    val bound = boundSyms(d)
    val r = readSyms(d).map {
      case Def(Reify(x,_,_)) => x
      case x => x
    }
    val notBound = r.filterNot(bound contains _)
    //if (d.isInstanceOf[Reify[Any]] && r.nonEmpty) {
    //  println("** actually read: "+readSyms(d)+"\\"+bound+"="+r)
    //  println("** transitive shallow: " + shallowAliases(r))
    //  println("** transitive deep: " + deepAliases(r))
    //}
    notBound
  }

  def readMutableData[A](d: Def[A]) = {
    val bound = boundSyms(d)
    mutableTransitiveAliases(getActuallyReadSyms(d)).filterNot(bound contains _)
  }

  protected[scalan] override def toExp[T](d: Def[T], newSym: => Exp[T]): Exp[T] = {
    super.toExp(d, newSym)
    /*
        are we depending on a variable or mutable object? then we need to be serialized -> effect

        the call chain goes like this:

          toAtom
          reflectEffect(Pure())      // figure out dependencies on mutable objects
          reflectEffectInternal(u)   // extended summary Pure() -> u
            super.toAtom             // if summary is still pure
            createReflectDefinition  // if summary is not pure
    */
    // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
    //if (manifest[T] == manifest[Any]) printlog("warning: possible missing mtype call - toAtom with Def of type Any " + d)

    // AKS NOTE: this was removed on 6/27/12, but it is still a problem in OptiML apps without it,
    // so I'm putting it back until we can get it resolved properly.
//    d match {
////      case Reify(x,_,_) =>
////        // aks: this became a problem after adding global mutable vars to the read deps list. what is the proper way of handling this?
////        // specifically, if we return the reified version of a mutable bound var, we get a Reflect(Reify(..)) error, e.g. mutable Sum
////        // printlog("ignoring read of Reify(): " + d)
////        super.toAtom(d)
//      case _ if conditionalScope && addControlDeps => reflectEffect(d, Control())
//      case _ => reflectEffect(d, Pure())
//    }
    // reflectEffect(d, Pure())
  }

  def reflectMirrored[A:Elem](zd: Reflect[A]): Exp[A] = {
    checkContext()
    createReflectDefinition(fresh[A], zd)
    // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
    //if (manifest[A] == manifest[Any]) printlog("warning: possible missing mtype call - reflectMirrored with Def of type Any: " + zd)
//    context.filter { case Def(d) if d == zd => true case _ => false }.reverse match {
//      //case z::_ => z.asInstanceOf[Exp[A]]  -- unsafe: we don't have a tight context, so we might pick one from a flattened subcontext
//      case _ => createReflectDefinition(fresh[A], zd)
//    }
  }

  def checkIllegalSharing(z: Exp[Any], mutableAliases: List[Exp[Any]]) {
    if (mutableAliases.nonEmpty) {
      val zd = z match { case Def(zd) => zd }
      printerr("error: illegal sharing of mutable objects " + mutableAliases.mkString(", "))
      printerr("at " + z + "=" + zd)
    }
  }

  def isWritableSym[A](w: Exp[A]): Boolean = {
    findDefinition(w) match {
      case Some(TableEntry(_, Reflect(_, u, _))) if mustMutable(u) => true // ok
      case o => globalMutableSyms.contains(w)
    }
  }


  var globalMutableSyms: List[Exp[Any]] = Nil

  def reflectMutableSym[A](s: Exp[A]): Exp[A] = {
    assert(findDefinition(s).isEmpty)
    globalMutableSyms = globalMutableSyms :+ s
    s
  }

  def reflectMutable[A:Elem](d: Def[A]): Exp[A] = {
    val z = reflectEffect(d, Alloc(), fresh[A])

    val mutableAliases = mutableTransitiveAliases(d)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def reflectWrite[A:Elem](write0: Exp[Any]*)(d: Def[A]): Exp[A] = {
    val write = write0.toList.asInstanceOf[List[Exp[Any]]] // should check...

    val z = reflectEffect(d, Write(write), fresh[A])

    val mutableAliases = mutableTransitiveAliases(d) filterNot (write contains _)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def createReflectDefinition[A](s: Exp[A], x: Reflect[A]): Exp[A] = {
    checkReflect(s, x)
    createDefinition(thunkStack.top, s, x)
    context :+= s
    s
  }

  def reflectEffect[A:Elem](x: Def[A]): Exp[A] =
    reflectEffect(x, Simple(), fresh[A]) // simple effect (serialized with respect to other simples)

  def reflectEffect[A:Elem](d: Def[A], u: Summary, newSym: => Exp[A]): Exp[A] = {
    // are we depending on a variable? then we need to be serialized -> effect
    //val mutableInputs = readMutableData(d)
    reflectEffectInternal(d, u /*andAlso Read(mutableInputs)*/, newSym) // will call super.toAtom if mutableInput.isEmpty
  }

  def reflectEffectInternal[A:Elem](x: Def[A], u: Summary, newSym: => Exp[A]): Exp[A] = {
    if (mustPure(u))
      super.toExp(x, newSym)
    else {
      checkContext()
      // NOTE: reflecting mutable stuff *during mirroring* doesn't work right now.

      // FIXME: Reflect(Reflect(ObjectUnsafeImmutable(..))) on delite
      assert(!x.isInstanceOf[Reflect[_]], x)

      val deps = calculateDependencies(u)
      val zd = Reflect(x,u,deps)
      if (mustIdempotent(u)) {
        context find { case Def(d) => d == zd } map { _.asInstanceOf[Exp[A]] } getOrElse {
          //        findDefinition(zd) map (_.sym) filter (context contains _) getOrElse { // local cse TODO: turn around and look at context first??
          val z = newSym
          if (!x.toString.startsWith("ReadVar")) { // supress output for ReadVar
            printlog("promoting to effect: " + z + "=" + zd)
            for (w <- u.mayRead)
              printlog("depends on  " + w)
          }
          createReflectDefinition(z, zd)
        }
      } else {
        val z = newSym
        // make sure all writes go to allocs
        for (w <- u.mayWrite if !isWritableSym(w)) {
          printerr("error: write to non-mutable " + w + " -> " + findDefinition(w))
          printerr("at " + z + "=" + zd)
        }
        // prevent sharing between mutable objects / disallow mutable escape for non read-only operations
        // make sure no mutable object becomes part of mutable result (in case of allocation)
        // or is written to another mutable object (in case of write)
        /*
          val a = mzeros(100)
          val b = zeros(100)
          val c = if (..) {
            a.update
            b
          } else {
            a
          }

          PROBLEM: the whole if expr has summary mayWrite=List(a), mstWrite=Nil and allAliases=List(a,b)

          what is the right thing?
          - mutableAliases \ mstWrite <-- first try, but maybe to restrictive?
          - mutableAliases \ mayWrite <-- too permissive?
          - something else?

        */
        createReflectDefinition(z, zd)
      }
    }
  }

  def calculateDependencies(u: Summary): State = {
    checkContext();
    calculateDependencies(context, u, true)
  }
  def calculateDependencies(scope: State, u: Summary, mayPrune: Boolean): State = {
    if (u.mayGlobal) scope else {
      val read = u.mayRead
      val write = u.mayWrite

      // TODO: in order to reduce the number of deps (need to traverse all those!)
      // we should only store those that are not transitively implied.
      // For simple effects, take the last one (implemented).
      // For mutations, take the last write to a particular mutable sym (TODO).

      def canonic(xs: List[Exp[Any]]) = xs // TODO
      def canonicLinear(xs: List[Exp[Any]]) = if (mayPrune) xs.takeRight(1) else xs

      // the mayPrune flag is for test8-speculative4: with pruning on, the 'previous iteration'
      // dummy is moved out of the loop. this is not per se a problem -- need to look some more into it.

      val readDeps = if (read.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, read) || read.contains(e) }
      val softWriteDeps = if (write.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayRead(u, write) }
      val writeDeps = if (write.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, write) || write.contains(e) }
      val simpleDeps = if (!u.maySimple) Nil else scope filter { case e@Def(Reflect(_, u, _)) => u.maySimple }
      val controlDeps = if (!u.control) Nil else scope filter { case e@Def(Reflect(_, u, _)) => u.control }
      val globalDeps = scope filter { case e@Def(Reflect(_, u, _)) => u.mayGlobal }

      // TODO: write-on-read deps should be weak
      // TODO: optimize!!
      val allDeps = canonic(readDeps ++ softWriteDeps ++ writeDeps ++ canonicLinear(simpleDeps) ++ canonicLinear(controlDeps) ++ canonicLinear(globalDeps))
      scope filter (allDeps contains _)
    }
  }

  def checkReflect[A](s: Exp[A], x: Reflect[A]) = x match {
    case Reflect(Reify(_,_,_),_,_) =>
      printerr("ERROR: reflecting a reify node.")
      printerr(s"at $s = $x")
    case _ => //ok
  }

  def checkContext() {
    if (context == null)
      sys.error("uninitialized effect context: effectful statements may only be used within a reifyEffects { .. } block")
  }


  // --- reify

  def summarizeAll(es: List[Exp[Any]]): Summary = {
    // compute an *external* summary for a seq of nodes
    // don't report any reads/writes on data allocated within the block
    var u = Pure()
    var ux = u
    var allocs: List[Exp[Any]] = Nil
    def clean(xs: List[Exp[Any]]) = xs.filterNot(allocs contains _)
    for (s@Def(Reflect(_, u2, _)) <- es) {
      if (mustMutable(u2)) allocs ::= s
      u = u andThen (u2.copy(mayRead = clean(u2.mayRead), mstRead = clean(u2.mstRead),
        mayWrite = clean(u2.mayWrite), mstWrite = clean(u2.mstWrite)))
      ux = ux andThen u2
    }
    //if (ux != u) printdbg("** effect summary reduced from "+ux+" to" + u)
    u
  }

  def pruneContext(ctx: List[Exp[Any]]): List[Exp[Any]] = ctx // TODO this doesn't work yet (because of loops!): filterNot { case Def(Reflect(_,u,_)) => mustOnlyRead(u) }

  // reify the effects of an isolated block.
  // no assumptions about the current context remain valid.
  def reifyEffects[A](block: => Exp[A], controlScope: Boolean = false): Block[A] = {
    val save = context
    context = Nil

    // only add control dependencies scopes where controlScope is explicitly true (i.e., the first-level of an IfThenElse)
    val saveControl = conditionalScope
    conditionalScope = controlScope

    val (result, defs) = reifySubGraph(block)
    reflectSubGraph(defs)
    implicit val eA = result.elem
    conditionalScope = saveControl

    val deps = context
    val summary = summarizeAll(deps)
    context = save

    if (deps.isEmpty && mustPure(summary)) Block(result) else Block(Reify(result, summary, pruneContext(deps))) // calls toAtom...
  }


  // reify the effects of a block that is executed 'here' (if it is executed at all).
  // all assumptions about the current context carry over unchanged.
  def reifyEffectsHere[A](block: => Exp[A], controlScope: Boolean = false): Block[A] = {
    val save = context
    if (save eq null)
      context = Nil

    val saveControl = conditionalScope
    conditionalScope = controlScope

    val (result, defs) = reifySubGraph(block)
    reflectSubGraph(defs)
    implicit val eA = result.elem

    conditionalScope = saveControl

    if ((save ne null) && context.take(save.length) != save) // TODO: use splitAt
      printerr("error: 'here' effects must leave outer information intact: " + save + " is not a prefix of " + context)

    val deps = if (save eq null) context else context.drop(save.length)

    val summary = summarizeAll(deps)
    context = save

    if (deps.isEmpty && mustPure(summary)) Block(result) else Block(Reify(result, summary, pruneContext(deps))) // calls toAtom...
  }


}
