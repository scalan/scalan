package scalan.community

import scalan.Interpreter

trait InterpreterCommunity extends Interpreter {
  override val seq: ScalanCommunitySeq //with VectorsDslSeq
  override val staged: ScalanCommunityExp //with VectorsDslExp
}
