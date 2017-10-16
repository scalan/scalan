package scalan.meta.serialization

import spray.json.{DefaultJsonProtocol, JsonFormat}

import scalan.meta.ScalanAst._

//trait TpeExprProtocolAbs extends DefaultJsonProtocol { self: ScalanJsonProtocolAbs =>
//  import self._
//  implicit val tpeFormat0: JsonFormat[STpeExpr]  = ???
//  implicit val tpeFormat1 : JsonFormat[STpeAnnotated]
//  implicit val tpeFormat2 : jsonFormat(STpeSingleton)
//  implicit val tpeFormat3 = jsonFormat1(STpeStruct)
//  implicit val tpeFormat4 = jsonFormat2(STpeSelectFromTT)
//  implicit val tpeFormat5 = jsonFormat0(STpeEmpty)
//  implicit def tpeFormat6 = jsonFormat1(STpeConst)
//  implicit val tpeFormat7 = jsonFormat2(STpeCompound)
//  implicit val tpeFormat8 = jsonFormat2(STpePrimitive)
//  implicit val tpeFormat9 = jsonFormat3(STpeMethod)
//  implicit val tpeFormat10 = jsonFormat2(STpeBind)
//  implicit val tpeFormat11 = jsonFormat1(STpeThis)
//  implicit val tpeFormat12 = jsonFormat1(STpeTuple)
//  implicit val tpeFormat13 = jsonFormat2(STpeExistential)
//  implicit val tpeFormat14 = jsonFormat2(STpeSingle)
//  implicit val tpeFormat15 = jsonFormat2(STpeFunc)
//  implicit val tpeFormat16 = jsonFormat2(STraitCall)
//  implicit val tpeFormat17 = jsonFormat2(STpeTypeBounds)
//}
//
//trait ExprProtocol extends DefaultJsonProtocol { self: ScalanJsonProtocol =>
//  implicit val exprFormat0: JsonFormat[SExpr]  = ???
//  implicit val exprFormat1 = jsonFormat4(SCase)
//  implicit val exprFormat2 = jsonFormat4(SSuper)
//  implicit val exprFormat3 = jsonFormat3(SFunc)
//  implicit val exprFormat4 = jsonFormat2(STuple)
//  implicit val exprFormat5 = jsonFormat3(SMatch)
//  implicit val exprFormat6 = jsonFormat3(SExprApply)
//  implicit val exprFormat7 = jsonFormat3(SAscr)
//  implicit val exprFormat8 = jsonFormat2(SConst)
//  implicit val exprFormat9 = jsonFormat2(STypeApply)
//  implicit val exprFormat10 = jsonFormat3(SBlock)
//  implicit val exprFormat11 = jsonFormat3(SSelect)
//  implicit val exprFormat12 = jsonFormat3(SAssign)
//  implicit val exprFormat13 = jsonFormat4(SIf)
//  implicit val exprFormat14 = jsonFormat3(SAnnotated)
//  implicit val exprFormat15 = jsonFormat1(SEmpty)
//  implicit val exprFormat16 = jsonFormat2(SConst)
//  implicit val exprFormat17 = jsonFormat2(SIdent)
//  implicit val exprFormat18 = jsonFormat4(SApply)
//  implicit val exprFormat19: JsonFormat[SBodyItem] = ???
//  implicit val exprFormat20 = jsonFormat2(SThis)
//  implicit val exprFormat21 = jsonFormat3(SContr)
//  implicit val exprFormat22 = jsonFormat2(SLiteral)
//
//}
//
//trait TpeExprProtocol extends DefaultJsonProtocol { self: ScalanJsonProtocol =>
//  import self._
//  implicit val tpeFormat0: JsonFormat[STpeExpr]  = ???
//  implicit val tpeFormat1 = jsonFormat2(STpeAnnotated)
//  implicit val tpeFormat2 = jsonFormat1(STpeSingleton)
//  implicit val tpeFormat3 = jsonFormat1(STpeStruct)
//  implicit val tpeFormat4 = jsonFormat2(STpeSelectFromTT)
//  implicit val tpeFormat5 = jsonFormat0(STpeEmpty)
//  implicit def tpeFormat6 = jsonFormat1(STpeConst)
//  implicit val tpeFormat7 = jsonFormat2(STpeCompound)
//  implicit val tpeFormat8 = jsonFormat2(STpePrimitive)
//  implicit val tpeFormat9 = jsonFormat3(STpeMethod)
//  implicit val tpeFormat10 = jsonFormat2(STpeBind)
//  implicit val tpeFormat11 = jsonFormat1(STpeThis)
//  implicit val tpeFormat12 = jsonFormat1(STpeTuple)
//  implicit val tpeFormat13 = jsonFormat2(STpeExistential)
//  implicit val tpeFormat14 = jsonFormat2(STpeSingle)
//  implicit val tpeFormat15 = jsonFormat2(STpeFunc)
//  implicit val tpeFormat16 = jsonFormat2(STraitCall)
//  implicit val tpeFormat17 = jsonFormat2(STpeTypeBounds)
//}
//
//trait ExprProtocol extends DefaultJsonProtocol { self: ScalanJsonProtocol =>
//  implicit val exprFormat0: JsonFormat[SExpr]  = ???
//  implicit val exprFormat1 = jsonFormat4(SCase)
//  implicit val exprFormat2 = jsonFormat4(SSuper)
//  implicit val exprFormat3 = jsonFormat3(SFunc)
//  implicit val exprFormat4 = jsonFormat2(STuple)
//  implicit val exprFormat5 = jsonFormat3(SMatch)
//  implicit val exprFormat6 = jsonFormat3(SExprApply)
//  implicit val exprFormat7 = jsonFormat3(SAscr)
//  implicit val exprFormat8 = jsonFormat2(SConst)
//  implicit val exprFormat9 = jsonFormat2(STypeApply)
//  implicit val exprFormat10 = jsonFormat3(SBlock)
//  implicit val exprFormat11 = jsonFormat3(SSelect)
//  implicit val exprFormat12 = jsonFormat3(SAssign)
//  implicit val exprFormat13 = jsonFormat4(SIf)
//  implicit val exprFormat14 = jsonFormat3(SAnnotated)
//  implicit val exprFormat15 = jsonFormat1(SEmpty)
//  implicit val exprFormat16 = jsonFormat2(SConst)
//  implicit val exprFormat17 = jsonFormat2(SIdent)
//  implicit val exprFormat18 = jsonFormat4(SApply)
//  implicit val exprFormat19: JsonFormat[SBodyItem] = ???
//  implicit val exprFormat20 = jsonFormat2(SThis)
//  implicit val exprFormat21 = jsonFormat3(SContr)
//  implicit val exprFormat22 = jsonFormat2(SLiteral)
//
//}
//
//trait ScalanJsonProtocol extends DefaultJsonProtocol with TpeExprProtocol with ExprProtocol {
//
//}

class SpireScalanSerializer[T] extends ScalanSerializer[T] {
  override def serialize(t: T): String = { ???
  }

  override def deserialize(fromString: String): T = { ??? }
}
