package scalan

trait Base {
  protected def nodeColor(optDef: Option[Option[_]]): String = ""

}
trait Child1 extends Base {
  override protected def nodeColor(optDef: Option[Option[_]]): String = super.nodeColor(optDef)

}
/**
 * Created by slesarenko on 17/01/15.
 */

trait Child2 extends Base {
  override protected def nodeColor(optDef: Option[Option[_]]): String = super.nodeColor(optDef)
}
trait ScalanExp
  extends Base
          with Child1
          with Child2
