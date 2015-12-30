package scalan.monads

import scalan.AbstractElemTests

class HigherKindContainerTests extends AbstractElemTests {
  class Ctx extends super.Ctx with MonadsDslExp

  it("Equality works as expected on containers with higher-kind type variables") {
    // this doesn't currently work
    pending

    val ctx = new Ctx
    import ctx._

    type CP_A_AB[A] = Coproduct[Array, ArrayBuffer, A]
    type CP_AB_A[A] = Coproduct[ArrayBuffer, Array, A]

    containersShouldBeEqual[CP_A_AB, CP_A_AB]
    containersShouldNotBeEqual[CP_A_AB, CP_AB_A]

    type SF_I[A] = StateF[Int, A]

    containersShouldBeEqual[SF_I, SF_I]
  }
}
