package scalan.primitives

import scalan.{Base, Scalan, Lazy}

trait Loops extends Base { self: Scalan =>

  def loopUntilAux[A](s1: Rep[A])(isMatch: Rep[A] => Rep[Boolean], step: Rep[A] => Rep[A]): Rep[A] = {
    val eA = s1.elem
    val leA = Lazy(eA)
    loopUntil(s1)(fun(isMatch)(leA), fun(step)(leA))
  }

  def loopUntil2[A, B](s1: Rep[A], s2: Rep[B])
                      (isMatch: (Rep[A],Rep[B]) => Rep[Boolean],
                       step: (Rep[A], Rep[B]) => (Rep[A], Rep[B])): Rep[(A,B)] = {
    loopUntilAux(Pair(s1, s2))({case Pair(a,b) => isMatch(a,b)}, {case Pair(a,b) => step(a,b)})
  }

  def loopUntil3[A, B, C](s1: Rep[A], s2: Rep[B], s3: Rep[C])
                         (isMatch: (Rep[A],Rep[B],Rep[C]) => Rep[Boolean],
                          step: (Rep[A], Rep[B], Rep[C]) => (Rep[A], Rep[B], Rep[C])): Rep[(A,(B,C))] = {
    loopUntilAux(Tuple(s1, s2, s3))({case Tuple(a,b,c) => isMatch(a,b,c) }, {case Tuple(a,b,c) => step(a,b,c)})
  }

  def loopUntil4[A, B, C, D](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D])
                            (isMatch: (Rep[A],Rep[B],Rep[C],Rep[D]) => Rep[Boolean],
                             step: (Rep[A], Rep[B], Rep[C], Rep[D]) => (Rep[A], Rep[B], Rep[C], Rep[D])): Rep[(A,(B,(C,D)))] = {
    loopUntilAux(Tuple(s1, s2, s3,s4))({case Tuple(a,b,c,d) => isMatch(a,b,c,d) }, {case Tuple(a,b,c,d) => step(a,b,c,d)})
  }

  def loopUntil5[A, B, C, D, E](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E])
                               (isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E]) => Rep[Boolean],
                                step: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]) => (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E])): Rep[(A,(B,(C,(D,E))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5))({case Tuple(a,b,c,d,e) => isMatch(a,b,c,d,e) }, {case Tuple(a,b,c,d,e) => step(a,b,c,d,e)})

  def loopUntil6[A, B, C, D, E, F](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F])
                                  (isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F]) => Rep[Boolean],
                                   step: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E],Rep[F]) => (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E],Rep[F])): Rep[(A,(B,(C,(D,(E,F)))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5,s6))({case Tuple(a,b,c,d,e,f) => isMatch(a,b,c,d,e,f) }, {case Tuple(a,b,c,d,e,f) => step(a,b,c,d,e,f)})

  def loopUntil7[A, B, C, D, E, F, G](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F], s7: Rep[G])
                                     (isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G]) => Rep[Boolean],
                                      step: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E],Rep[F],Rep[G]) => (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E],Rep[F],Rep[G])): Rep[(A,(B,(C,(D,(E,(F,G))))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5,s6,s7))({case Tuple(a,b,c,d,e,f,g) => isMatch(a,b,c,d,e,f,g) }, {case Tuple(a,b,c,d,e,f,g) => step(a,b,c,d,e,f,g)})

  def loopUntil8[A, B, C, D, E, F, G, H](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F], s7: Rep[G], s8: Rep[H])
                                        (isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H]) => Rep[Boolean],
                                         step: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E],Rep[F],Rep[G],Rep[H]) => (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E],Rep[F],Rep[G],Rep[H])): Rep[(A,(B,(C,(D,(E,(F,(G,H)))))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5, s6, s7, s8))({case Tuple(a,b,c,d,e,f,g,h) => isMatch(a,b,c,d,e,f,g,h) }, {case Tuple(a,b,c,d,e,f,g,h) => step(a,b,c,d,e,f,g,h)})

  def from[A](s1: Rep[A]) = new From1(s1)
  def from[A, B](s1: Rep[A], s2: Rep[B]) = new From2(s1, s2)
  def from[A, B, C](s1: Rep[A], s2: Rep[B], s3: Rep[C]) =
    new From3(s1, s2, s3)
  def from[A, B, C, D](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D]) =
    new From4(s1, s2, s3, s4)
  def from[A, B, C, D, E](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E]) =
    new From5(s1, s2, s3, s4, s5)
  def from[A, B, C, D, E, F](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F]) =
    new From6(s1, s2, s3, s4, s5, s6)
  def from[A, B, C, D, E, F,G](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F], s7:Rep[G]) =
    new From7(s1, s2, s3, s4, s5, s6, s7)
  def from[A, B, C, D, E, F, G, H](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F], s7:Rep[G], s8:Rep[H]) =
    new From8(s1, s2, s3, s4, s5, s6, s7, s8)

  class From1[A](s1: Rep[A]) {
    def until(isMatch: Rep[A] => Rep[Boolean])(step: Rep[A] => Rep[A]) =
      loopUntilAux(s1)({case a => isMatch(a) }, {case a => step(a)})
  }
  class From2[A, B](s1: Rep[A], s2: Rep[B]) {
    def until(isMatch: (Rep[A],Rep[B]) => Rep[Boolean])(step: (Rep[A], Rep[B]) => (Rep[A], Rep[B])) =
      loopUntil2(s1, s2)(isMatch, step)
  }
  class From3[A, B, C](s1: Rep[A], s2: Rep[B], s3: Rep[C]) {
    def until(isMatch: (Rep[A],Rep[B],Rep[C]) => Rep[Boolean])(step: (Rep[A],Rep[B],Rep[C]) => (Rep[A],Rep[B],Rep[C])) =
      loopUntil3(s1, s2, s3)(isMatch, step)
  }
  class From4[A, B, C, D](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D]) {
    def until(isMatch: (Rep[A],Rep[B],Rep[C],Rep[D]) => Rep[Boolean])(step: (Rep[A],Rep[B],Rep[C],Rep[D]) => (Rep[A],Rep[B],Rep[C],Rep[D])) =
      loopUntil4(s1, s2, s3, s4)(isMatch, step)
  }
  class From5[A, B, C, D, E](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E]) {
    def until(isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E]) => Rep[Boolean])(step: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E]) => (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E])) =
      loopUntil5(s1, s2, s3, s4, s5)(isMatch, step)
  }
  class From6[A, B, C, D, E, F](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F]) {
    def until(isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F]) => Rep[Boolean])(step: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F]) => (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F])) =
      loopUntil6(s1, s2, s3, s4, s5,s6)(isMatch, step)
  }
  class From7[A, B, C, D, E, F,G](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F], s7:Rep[G]) {
    def until(isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G]) => Rep[Boolean])(step: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G]) => (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G])) =
      loopUntil7(s1, s2, s3, s4, s5,s6,s7)(isMatch, step)
  }
  class From8[A, B, C, D, E, F, G, H](s1: Rep[A], s2: Rep[B], s3: Rep[C], s4: Rep[D], s5: Rep[E], s6: Rep[F], s7:Rep[G], s8: Rep[H]) {
    def until(isMatch: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H]) => Rep[Boolean])(step: (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H]) => (Rep[A],Rep[B],Rep[C],Rep[D],Rep[E],Rep[F],Rep[G],Rep[H])) =
      loopUntil8(s1, s2, s3, s4, s5, s6, s7, s8)(isMatch, step)
  }

  def loopUntil[A](s1: Rep[A])(isMatch: Rep[A => Boolean], step: Rep[A => A]): Rep[A] = LoopUntil(s1, step, isMatch)

  case class LoopUntil[A](s1: Rep[A], step: Rep[A => A], isMatch: Rep[A => Boolean]) extends Def[A] {
    lazy val selfType = s1.elem.leastUpperBound(step.elem.eRange).asElem[A]
    override def productIterator = List(step, isMatch, s1).toIterator
  }
}
