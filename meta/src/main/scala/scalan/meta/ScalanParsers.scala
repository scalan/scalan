/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package scalan.meta

import scala.language.implicitConversions
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.OffsetPosition
import scalan.meta.ScalanAst._

trait ScalanParsers {

  type Compiler <: scala.tools.nsc.Global
  val compiler: Compiler
  import compiler._

  implicit def nameToString(name: Name): String = name.toString

  implicit class OptionListOps[A](opt: Option[List[A]]) {
    def flatList: List[A] = opt.toList.flatten
  }

  private def positionString(tree: Tree) = {
    tree.pos match {
      case pos: RangePosition =>
        val path = pos.source.file.canonicalPath
        s"file $path at ${pos.line}:${pos.column} (start ${pos.point - pos.start} before, end ${pos.end - pos.point} after)"
      case pos: OffsetPosition =>
        val path = pos.source.file.canonicalPath
        s"file $path at ${pos.line}:${pos.column}"
      case pos => pos.toString
    }
  }

  def !!!(msg: String, tree: Tree) = {
    val fullMsg = s"$msg at ${positionString(tree)}"
    throw new IllegalStateException(fullMsg)
  }

  def !!!(msg: String) = Base.!!!(msg)

  def ???(tree: Tree) = {
    val pos = tree.pos
    val msg = s"Unhandled case in ${positionString(tree)}:\nAST: ${showRaw(tree)}\n\nCode for AST: $tree"
    throw new IllegalStateException(msg)
  }

  def parse(name: String, tree: Tree) = tree match {
    case pd: PackageDef =>
      entityModule(pd)
    case tree =>
      throw new Exception(s"Unexpected tree in $name:\n\n$tree")
  }

  def config: CodegenConfig

  def seqImplementation(traitTree: ClassDef) = {
    val methods = traitTree.impl.body.collect { case item: DefDef => item }

    SSeqImplementation(methods.map(methodDef(_)))
  }

  def entityModule(fileTree: PackageDef) = {
    val packageName = fileTree.pid.toString
    val statements = fileTree.stats
    val imports = statements.collect {
      case i: Import => importStat(i)
    }
    val moduleTraitTree = statements.collect {
      case cd: ClassDef if cd.mods.isTrait && !cd.name.contains("Dsl") => cd
    } match {
      case Seq(only) => only
      case seq => !!!(s"There must be exactly one module trait in file, found ${seq.length}")
    }
    val moduleTrait = traitDef(moduleTraitTree, Some(moduleTraitTree))
    val moduleName = moduleTrait.name

    def findClassDefByName(trees: List[Tree], name: String) =
      trees.collectFirst {
        case cd: ClassDef if cd.name.toString == name => cd
      }

    val hasDsl =
      findClassDefByName(fileTree.stats, moduleName + "Dsl").isDefined
    
    val dslSeqOpt = findClassDefByName(fileTree.stats, moduleName + "DslSeq")
    val hasDslSeq = dslSeqOpt.isDefined

    val hasDslExp =
      findClassDefByName(fileTree.stats, moduleName + "DslExp").isDefined

    val defs = moduleTrait.body

    val entityRepSynonym = defs.collectFirst { case t: STpeDef => t }

    val traits = defs.collect { case t: STraitDef if !t.name.endsWith("Companion") => t }
    val entity = traits.headOption.getOrElse {
      throw new IllegalStateException(s"Invalid syntax of entity module trait $moduleName. First member trait must define the entity, but no member traits found.")
    }

    def tpeUseExpr(arg: STpeArg): STpeExpr = STraitCall(arg.name, arg.tparams.map(tpeUseExpr(_)))

    val classes = entity.optBaseType match {
      case Some(bt) =>
        val entityName = entity.name
        val entityImplName = entityName + "Impl"
        val typeUseExprs = entity.tpeArgs.map(tpeUseExpr(_))
        val defaultBTImpl = SClassDef(
          name = entityImplName,
          tpeArgs = entity.tpeArgs,
          args = SClassArgs(List(SClassArg(false, false, true, "wrappedValueOfBaseType", STraitCall("Rep", List(bt)), None))),
          implicitArgs = entity.implicitArgs,
          ancestors = List(STraitCall(entity.name, typeUseExprs)),
          body = List(

          ),
          selfType = None,
          companion = None,
          //            companion = defs.collectFirst {
          //              case c: STraitOrClassDef if c.name.toString == entityImplName + "Companion" => c
          //            },
          true, Nil

        )
        defaultBTImpl :: moduleTrait.getConcreteClasses
      case None => moduleTrait.getConcreteClasses
    }
    val methods = defs.collect { case md: SMethodDef => md }

    val seqImplementation = for {
      dslSeq <- dslSeqOpt
      seqOpsTrait <- findClassDefByName(dslSeq.impl.body, "Seq" + entity.name)
    } yield {
      this.seqImplementation(seqOpsTrait)
    }

    SEntityModuleDef(packageName, imports, moduleName,
      entityRepSynonym, entity, traits, classes, methods,
      moduleTrait.selfType, Nil, seqImplementation,
      hasDsl, hasDslSeq, hasDslExp, moduleTrait.ancestors)
  }

  def importStat(i: Import): SImportStat = {
    SImportStat(i.toString.stripPrefix("import "))
  }

  def isEvidenceParam(vd: ValDef) = vd.name.toString.startsWith("evidence$")

  def tpeArgs(typeParams: List[TypeDef], possibleImplicits: List[ValDef]): List[STpeArg] = {
    val evidenceTypes = possibleImplicits.filter(isEvidenceParam(_)).map(_.tpt)

    def tpeArg(tdTree: TypeDef): STpeArg = {
      val bound = tdTree.rhs match {
        case TypeBoundsTree(low, high) =>
          if (high.toString == "_root_.scala.Any")
            None
          else
            optTpeExpr(high)
        case tt: TypeTree => parseType(tt.tpe) match {
          case STpeTypeBounds(_, STpePrimitive("Any", _)) => None
          case STpeTypeBounds(_, hi) => Some(hi)
          case tpe => ???(tdTree)
        }
        case _ => ???(tdTree)
      }
      val contextBounds = evidenceTypes.collect {
        case AppliedTypeTree(tpt, List(arg)) if arg.toString == tdTree.name.toString =>
          Some(tpt.toString)
        case _ => None
      }.flatten
      val tparams = tdTree.tparams.map(tpeArg)
      STpeArg(tdTree.name, bound, contextBounds, tparams, tdTree.mods.flags)
    }

    typeParams.map(tpeArg)
  }

  // exclude default parent
  def ancestors(trees: List[Tree]) = trees.map(traitCall).filter(tr => !Set("AnyRef", "scala.AnyRef").contains(tr.name))

  def findCompanion(name: String, parentScope: Option[ImplDef]) = parentScope match {
    case Some(scope) => scope.impl.body.collect {
      case c: ClassDef if config.isAlreadyRep && c.name.toString == name + "Companion" =>
        if (c.mods.isTrait) traitDef(c, parentScope) else classDef(c, parentScope)
      case m: ModuleDef if !config.isAlreadyRep && !m.mods.isSynthetic && m.name.toString == name => objectDef(m)
    }.headOption
    case None => None
  }

  def traitDef(td: ClassDef, parentScope: Option[ImplDef]): STraitDef = {
    val tpeArgs = this.tpeArgs(td.tparams, Nil)
    val ancestors = this.ancestors(td.impl.parents)
    val body = td.impl.body.flatMap(optBodyItem(_, Some(td)))
    val selfType = this.selfType(td.impl.self)
    val name = td.name.toString
    val companion = findCompanion(name, parentScope)
    val annotations = parseAnnotations(td)((n,as) => STraitOrClassAnnotation(n,as.map(parseExpr)))
    STraitDef(name, tpeArgs, ancestors, body, selfType, companion, annotations)
  }

  def classDef(cd: ClassDef, parentScope: Option[ImplDef]): SClassDef = {
    val ancestors = this.ancestors(cd.impl.parents)
    val constructor = (cd.impl.body.collect {
      case dd: DefDef if dd.name == nme.CONSTRUCTOR => dd
    }) match {
      case Seq(only) => only
      case seq => !!!(s"Class ${cd.name} should have 1 constructor but has ${seq.length} constructors", cd)
    }
    // TODO simplify
    val (args, implicitArgs) = constructor.vparamss match {
      case Seq() =>
        (classArgs(List.empty), classArgs(List.empty))
      case Seq(nonImplConArgs) =>
        (classArgs(nonImplConArgs), classArgs(List.empty))
      case Seq(nonImplConArgs, implConArgs) =>
        (classArgs(nonImplConArgs), classArgs(implConArgs))
      case seq => !!!(s"Constructor of class ${cd.name} has more than 2 parameter lists, not supported")
    }
    val tpeArgs = this.tpeArgs(cd.tparams, constructor.vparamss.lastOption.getOrElse(Nil))
    val body = cd.impl.body.flatMap(optBodyItem(_, Some(cd)))
    val selfType = this.selfType(cd.impl.self)
    val isAbstract = cd.mods.hasAbstractFlag
    val name = cd.name.toString
    val companion = findCompanion(name, parentScope)
    val annotations = parseAnnotations(cd)((n,as) => STraitOrClassAnnotation(n,as.map(parseExpr)))
    SClassDef(cd.name, tpeArgs, args, implicitArgs, ancestors, body, selfType, companion, isAbstract, annotations)
  }

  def objectDef(od: ModuleDef): SObjectDef = {
    val ancestors = this.ancestors(od.impl.parents)
    val body = od.impl.body.flatMap(optBodyItem(_, Some(od)))
    SObjectDef(od.name, ancestors, body)
  }

  def classArgs(vds: List[ValDef]): SClassArgs = SClassArgs(vds.filter(!isEvidenceParam(_)).map(classArg))

  def classArg(vd: ValDef): SClassArg = {
    val tpe = tpeExpr(vd.tpt)
    val default = optExpr(vd.rhs)
    val isOverride = vd.mods.isAnyOverride
    val isVal = vd.mods.isParamAccessor
    val annotations = parseAnnotations(vd)((n,as) => new SArgAnnotation(n, as.map(parseExpr)))
    SClassArg(vd.mods.isImplicit, isOverride, isVal, vd.name, tpe, default, annotations)
  }

  def traitCall(tree: Tree): STraitCall = tree match {
    case ident: Ident =>
      STraitCall(ident.name, List())
    case select: Select =>
      STraitCall(select.name, List())
    case AppliedTypeTree(tpt, args) =>
      STraitCall(tpt.toString, args.map(tpeExpr))
    case tt: TypeTree =>
      val parsedType = parseType(tt.tpe)
      if (parsedType.isInstanceOf[STraitCall]) parsedType.asInstanceOf[STraitCall]
      else throw new IllegalArgumentException(parsedType.toString)
    case tree => ???(tree)
  }

  def isExplicitMethod(md: DefDef): Boolean = {
    if (nme.isConstructorName(md.name)) false
    else if (md.mods.isSynthetic) false
    else if (md.mods.isCaseAccessor) false
    else if (md.mods.isParamAccessor) false
    else true
  }

  def optBodyItem(tree: Tree, parentScope: Option[ImplDef]): Option[SBodyItem] = tree match {
    case i: Import =>
      Some(importStat(i))
    case md: DefDef =>
      if (isExplicitMethod(md))
        md.tpt match {
          case AppliedTypeTree(tpt, _) if tpt.toString == "Elem" || tpt.toString == "Element" =>
            Some(methodDef(md, true))
          case _ =>
            Some(methodDef(md))
        }
      else
        None
    case td: TypeDef =>
      val tpeArgs = this.tpeArgs(td.tparams, Nil)
      val rhs = tpeExpr(td.rhs)
      Some(STpeDef(td.name, tpeArgs, rhs))
    case td: ClassDef if td.mods.isTrait =>
      Some(traitDef(td, parentScope))
    case cd: ClassDef if !cd.mods.isTrait =>
      // don't include implicit conversion classes
      if (!cd.mods.isImplicit)
        Some(classDef(cd, parentScope))
      else
        None
    case od: ModuleDef =>
      Some(objectDef(od))
    case vd: ValDef =>
      if (!vd.mods.isParamAccessor) {
        val tpeRes = optTpeExpr(vd.tpt)
        val isImplicit = vd.mods.isImplicit
        val isLazy = vd.mods.isLazy
        Some(SValDef(vd.name, tpeRes, isLazy, isImplicit, parseExpr(vd.rhs)))
      } else
        None
    case EmptyTree =>
      None
    case tree => ???(tree)
  }

  object ExtractAnnotation {
    def unapply(a: Tree): Option[(String, List[Tree])] = a match {
      case Apply(Select(New(Ident(ident)), nme.CONSTRUCTOR), args) => Some((ident, args))
      case _ => None
    }
  }

  def parseAnnotations[A <: SAnnotation](md: MemberDef)(p: (String, List[Tree]) => A): List[A] = {
    val annotations = md.mods.annotations.map {
      case ExtractAnnotation (name, args) => p(name, args)
      case a => !!! (s"Cannot parse annotation $a of MemberDef $md")
    }
    annotations
  }

  class HasAnnotation(annClass: String) {
    def unapply(md: MemberDef): Option[List[Tree]] =
      md.mods.annotations.collectFirst {
        case ExtractAnnotation(name, args) if name == annClass => args
      }
  }

  //  val HasExternalAnnotation = new ExtractAnnotation("External")
  //  val HasConstructorAnnotation = new ExtractAnnotation("Constructor")
  val HasArgListAnnotation = new HasAnnotation("ArgList")
  val OverloadIdAnnotation = new HasAnnotation("OverloadId")

  def methodDef(md: DefDef, isElem: Boolean = false) = {
    val tpeArgs = this.tpeArgs(md.tparams, md.vparamss.lastOption.getOrElse(Nil))
    val args0 = md.vparamss.map(methodArgs)
    val args = if (!args0.isEmpty && args0.last.args.isEmpty) args0.init else args0
    val tpeRes = optTpeExpr(md.tpt)
    val isImplicit = md.mods.isImplicit
    val isOverride = md.mods.isOverride
    val optOverloadId = md match {
      case OverloadIdAnnotation(List(Literal(Constant(overloadId)))) =>
        Some(overloadId.toString)
      case _ => None
    }
    val annotations = md.mods.annotations.map {
      case ExtractAnnotation(name, args) => SMethodAnnotation(name, args.map(parseExpr))
      case a => !!!(s"Cannot parse annotation $a of the method $md")
    }
    //    val optExternal = md match {
    //      case HasExternalAnnotation(_) => Some(ExternalMethod)
    //      case HasConstructorAnnotation(_) => Some(ExternalConstructor)
    //      case _ => None
    //    }
    val optBody:Option[SExpr] = md.rhs match {
      case Apply(ident:Ident, args) if ident.name.intern() == "sql" =>
        Some(SApply(SLiteral("sql"), List(), List(List(SLiteral(args(0).asInstanceOf[Literal].value.stringValue)))))
      case _ => optExpr(md.rhs)
    }
    val isElemOrCont = md.tpt match {
      case AppliedTypeTree(tpt, _) if Set("Elem", "Element", "Cont", "Container").contains(tpt.toString) =>
        true
      case _ =>
        false
    }

    SMethodDef(md.name, tpeArgs, args, tpeRes, isImplicit, isOverride,
      optOverloadId, annotations, optBody, isElemOrCont)
  }

  def methodArgs(vds: List[ValDef]): SMethodArgs = vds match {
    case Nil => SMethodArgs(List.empty)
    case vd :: _ =>
      SMethodArgs(vds.filter(!isEvidenceParam(_)).map(methodArg))
  }

  def optTpeExpr(tree: Tree): Option[STpeExpr] = {
    tree match {
      case _ if tree.isEmpty => None
      case _: ExistentialTypeTree => None
      case tree => Some(tpeExpr(tree))
    }
  }

  def formAppliedTypeTree(fullName: String, shortName: String, argTpeExprs: List[STpeExpr]) = {
    if (fullName.contains("scala.Tuple"))
      STpeTuple(argTpeExprs)
    else if (fullName.contains("scala.Function")) {
      val domainTpeExpr = argTpeExprs.length match {
        case 2 => argTpeExprs(0)
        case n => STpeTuple(argTpeExprs.init)
      }
      STpeFunc(domainTpeExpr, argTpeExprs.last)
    } else
      STraitCall(shortName, argTpeExprs)
  }

  def tpeExpr(tree: Tree): STpeExpr = tree match {
    case EmptyTree => STpeEmpty()
    case ident: Ident =>
      val name = ident.name.toString
      STpePrimitives.getOrElse(name, STraitCall(name, List()))
    case select: Select =>
      STraitCall(select.name, List())
    case AppliedTypeTree(tpt, args) =>
      val argTpeExprs = args.map(tpeExpr)
      val genericTypeString = tpt.toString
      formAppliedTypeTree(genericTypeString, genericTypeString, argTpeExprs)
    case tq"$tpt @$annot" => STpeAnnotated(tpeExpr(tpt), annot.toString)
    case TypeBoundsTree(lo, hi) => STpeTypeBounds(tpeExpr(lo), tpeExpr(hi))
    case SingletonTypeTree(ref) => STpeSingleton(parseExpr(ref))
    case SelectFromTypeTree(qualifier, TypeName(name)) => STpeSelectFromTT(tpeExpr(qualifier), name)
    case tq"..$parents { ..$defns }" => STpeCompound(parents.map(tpeExpr), defns.flatMap(defn => optBodyItem(defn, None)))
    case tq"$tpt forSome { ..$defns }" => STpeExistential(tpeExpr(tpt), defns.flatMap(defn => optBodyItem(defn, None)))
    case Bind(TypeName(name), body) => STpeBind(name, tpeExpr(body))
    case tt: TypeTree => parseType(tt.tpe)
    case tree => ???(tree)
  }

  def methodArg(vd: ValDef): SMethodArg = {
    val tpe = tpeExpr(vd.tpt)
    val default = optExpr(vd.rhs)
    val annotations = parseAnnotations(vd)((n,as) => new SArgAnnotation(n, as.map(parseExpr)))
    val isOverride = vd.mods.isAnyOverride
    val isElemOrCont = tpe match {
      case STraitCall(tname, _) if tname == "Elem" || tname == "Cont" => true
      case _ => false
    }
    SMethodArg(vd.mods.isImplicit, isOverride, vd.name, tpe, default, annotations, isElemOrCont)
  }

  def selfType(vd: ValDef): Option[SSelfTypeDef] = {
    val components = vd.tpt match {
      case t if t.isEmpty =>
        Nil
      case CompoundTypeTree(Template(ancestors, _, _)) =>
        ancestors.map(tpeExpr)
      case t =>
        List(tpeExpr(t))
    }

    if (components.isEmpty)
      None
    else
      Some(SSelfTypeDef(vd.name.toString, components))
  }

  def optExpr(tree: Tree): Option[SExpr] = {
    if (tree.isEmpty)
      None
    else
      Some(parseExpr(tree))
  }

  def tree2Type(tree: Tree): Option[STpeExpr] = tree.tpe match {
    case null => None
    case tpe => Some(parseType(tpe))
  }

  def parseExpr(tree: Tree): SExpr = tree match {
    case EmptyTree => SEmpty(tree2Type(tree))
    case Literal(Constant(c)) => SConst(c, tree2Type(tree))
    case Ident(TermName(name)) => SIdent(name, tree2Type(tree))
    case q"$left = $right" => SAssign(parseExpr(left), parseExpr(right), tree2Type(tree))
    case q"$name.super[$qual].$field" => SSuper(name, qual, field, tree2Type(tree))
    case q"$expr.$tname" => SSelect(parseExpr(expr), tname, tree2Type(tree))
    case Apply(Select(New(name), termNames.CONSTRUCTOR), args) =>
      SContr(name.toString(), args.map(parseExpr), tree2Type(tree))
    case Apply(Select(Ident(TermName("scala")), TermName(tuple)), args) if tuple.startsWith("Tuple") =>
      STuple(args.map(parseExpr), tree2Type(tree))
    case Block(init, last) => SBlock(init.map(parseExpr), parseExpr(last), tree2Type(tree))
    case q"$mods val $tname: $tpt = $expr" =>
      SValDef(tname, optTpeExpr(tpt), mods.isLazy, mods.isImplicit, parseExpr(expr))
    case q"if ($cond) $th else $el" =>
      SIf(parseExpr(cond), parseExpr(th), parseExpr(el), tree2Type(tree))
    case q"$expr: $tpt" => SAscr(parseExpr(expr), tpeExpr(tpt), tree2Type(tree))
    case q"(..$params) => $expr" =>
      SFunc(params.map(param => parseExpr(param).asInstanceOf[SValDef]), parseExpr(expr), Some(parseType(tree.tpe)))
    case q"$tpname.this" => SThis(tpname, tree2Type(tree))
    case q"$expr: @$annot" => SAnnotated(parseExpr(expr), annot.toString, tree2Type(tree))
    case TypeApply(fun: Tree, args: List[Tree]) =>
      STypeApply(parseExpr(fun), args.map(tpeExpr), tree2Type(tree))
    case q"$expr match { case ..$cases } " => parseMatch(expr, cases)
    case q"{ case ..$cases }" => parseMatch(EmptyTree, cases)
    case Apply(TypeApply(fun, targs), args) =>
      SApply(parseExpr(fun), targs.map(tpeExpr), List(args.map(parseExpr)), tree2Type(tree))
    case Apply(fun, args) =>
      SApply(parseExpr(fun), Nil, List(args.map(parseExpr)), tree2Type(tree))
    case bi => optBodyItem(bi, None) match {
      case Some(item) => item
      case None => throw new NotImplementedError(s"parseExpr: Error parsing of ${showRaw(bi)}")
    }
  }

  def parseMatch(expr: Tree, cases: List[CaseDef]) = {
    SMatch(parseExpr(expr), cases.map{_ match {
      case cq"$pat if $guard => $body" => SCase(parsePattern(pat), parseExpr(guard), parseExpr(body))
      case c => throw new NotImplementedError(s"parseExpr: match {case ${showRaw(c)}")
    }})
  }

  object WildcardPattern {
    def unapply(pat: Tree): Boolean = pat match {
      case Bind(nme.WILDCARD, WildcardPattern()) => true
      case Star(WildcardPattern())               => true
      case x: Ident                              => treeInfo.isVarPattern(x)
      case Alternative(ps)                       => ps forall unapply
      case EmptyTree                             => true
      case _                                     => false
    }
  }

  def parsePattern(pat: Tree): SPattern = pat match {
    case WildcardPattern() => SWildcardPattern()
    case Apply(fun, pats) => SApplyPattern(parseExpr(fun), pats.map(parsePattern))
    case Typed(Ident(termNames.WILDCARD), tpe) => STypedPattern(tpeExpr(tpe))
    case Bind(TermName(name), expr) => SBindPattern(name, parsePattern(expr))
    case Literal(Constant(c)) => SLiteralPattern(SConst(c))
    case Ident(id) => SStableIdPattern(SIdent(id.toString))
    case Select(qual, name) => SSelPattern(parseExpr(qual), name.toString)
    case Alternative(alts) => SAltPattern(alts.map(parsePattern))
    case _ => throw new NotImplementedError(s"parsePattern: ${showRaw(pat)}")
  }

  def parseType(tpe: Type): STpeExpr = tpe match {
    case NoType | NoPrefix => STpeEmpty()
    case const: ConstantType => STpeConst(const.value.value)
    case thisType: ThisType => STpeThis(thisType.sym.nameString)
    case tref: TypeRef => parseTypeRef(tref)
    case single: SingleType => STpeSingle(parseType(single.pre), single.sym.nameString)
    case TypeBounds(lo, hi) => STpeTypeBounds(parseType(lo), parseType(hi))
    case ExistentialType(quant, under) =>
      val quantified = quant map(q => STpeDef(q.nameString, Nil, STpeEmpty()))
      val underlying = parseType(under)
      STpeExistential(underlying, quantified)
    case m: MethodType => parseMethodType(Nil, m)
    case PolyType(tparams, m: MethodType) => parseMethodType(tparams, m)
    case tpe => throw new NotImplementedError(showRaw(tpe, printTypes = Some(true)))
  }

  def parseMethodType(tparams: List[Symbol], m: MethodType): STpeMethod = {
    val method = m //uncurry.transformInfo(m.typeSymbol, m)
    val typeParams = tparams.map(_.nameString)
    val params = method.params.map(param => parseType(param.tpe))
    val res = parseType(method.resultType)

    STpeMethod(typeParams, params, res)
  }

  def parseTypeRef(tref: TypeRef): STpeExpr = {
    STpePrimitives.get(tref.sym.nameString) match {
      case Some(prim) => prim
      case None =>
        val fullName = tref.sym.fullNameString
        val shortName = tref.sym.nameString
        val args = tref.args map parseType

        formAppliedTypeTree(fullName, shortName, args)
    }
  }
}