package scalan.rx

import scalan.{ScalanStaged, ScalanSeqImplementation}

trait ReactiveOps { scalan: ReactiveDsl =>

  trait ObservableOps[A] extends Observable[A] {
  }
  trait ObservableImplOps[A] extends ObservableOps[A] {

  }
  trait ObservableImplCompanion {

  }
}

trait ReactiveDsl extends ReactiveAbs with ReactiveOps

trait ReactiveDslSeq extends ReactiveDsl with ReactiveSeq with ScalanSeqImplementation

trait ReactiveDslExp extends ReactiveDsl with ReactiveExp with ScalanStaged