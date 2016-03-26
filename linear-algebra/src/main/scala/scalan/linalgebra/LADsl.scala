package scalan.linalgebra

import scalan.collections.{CollectionsDsl, CollectionsDslStd, CollectionsDslExp}

trait LADsl extends CollectionsDsl
                with MatricesDsl with VectorsDsl

trait LADslStd extends CollectionsDslStd
                with LADsl with MatricesDslStd with VectorsDslStd

trait LADslExp extends CollectionsDslExp
                with LADsl with MatricesDslExp with VectorsDslExp
