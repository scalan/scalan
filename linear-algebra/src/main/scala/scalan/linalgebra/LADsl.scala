package scalan.linalgebra

import scalan.{ScalanDsl, ScalanDslStd, ScalanDslExp}

/**
 * Created by Victor Smirnov on 12/25/15.
 */

trait LADsl extends ScalanDsl
                with MatricesDsl with VectorsDsl

trait LADslStd extends ScalanDslStd
                with LADsl with MatricesDslStd with VectorsDslStd

trait LADslExp extends ScalanDslExp
                with LADsl with MatricesDslExp with VectorsDslExp
