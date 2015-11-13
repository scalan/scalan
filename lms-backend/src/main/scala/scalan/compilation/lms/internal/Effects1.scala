package scalan.compilation.lms.internal

import scala.lms.internal.Effects

// Required for some tests in StateItTests
// TODO Determine if it should be pull-requested to LMS
trait Effects1 extends Effects {
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case Reify(x, u, es) =>
      //      // in general: the result of a block is not read but passed through.
      //      // FIXME this piece of logic is not clear. is it a special case for unit??
      //      // it looks like this was introduced to prevent the Reify to be reflected
      //      // if x is a mutable object defined within the block.
      //      // TODO the globalMutableSyms part was added later (June 2012) -- make sure it does the right thing
      //      The below if...else part isn't commented out in LMS
      //      if ((es contains x) || (globalMutableSyms contains x)) Nil
      //      else
      readSyms(x)
    case _ => super.readSyms(e)
  }
}
