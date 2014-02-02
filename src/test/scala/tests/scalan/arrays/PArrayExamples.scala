/**
 * User: Alexander Slesarenko
 * Date: 11/24/13
 */
package tests.scalan.arrays

import scalan.ScalanDsl
import scalan.arrays.PArraysDsl

trait PArrayExamples extends ScalanDsl with PArraysDsl {
  lazy val fromAndTo = fun { xs: RA[(Int,Float)] => PArray(xs).arr }
}
