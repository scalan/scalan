/**
 * User: Alexander Slesarenko
 * Date: 11/23/13
 */
package scalan.arrays

import scalan._
import scalan.staged.BaseExp

trait ArrayViews extends ArrayOps with Views { self: Scalan => }

trait ArrayViewsStd extends ArrayViews with ArrayOpsStd with ViewsDslStd { self: ScalanStd => }

trait ArrayViewsExp extends ArrayViews with ArrayOpsExp with ViewsDslExp with BaseExp { self: ScalanExp =>
  
  case class ViewArray[A, B](source: Arr[A], override val innerIso: Iso[A, B])
    extends View1[A, B, Array](arrayIso(innerIso)) {
    override def toString = s"ViewArray[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewArray[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object UserTypeArray {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case ScalaArrayElem(UnpackableElem(iso)) => Some(iso)
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(zip: ArrayZip[a, b]) if shouldUnpackTuples =>
      val xs1 = zip.xs
      val ys1 = zip.ys
      implicit val eA = xs1.elem.eItem
      implicit val eB = ys1.elem.eItem
      val eAB = element[(a,b)]
      val eVal = tuple2StructElement[a,b]
      val eValSchema = tupleStructElement(xs1.elem, ys1.elem)
      val arr = arrayStruct(tupleStruct(xs1, ys1))(eVal, eValSchema)
      val innerIso = structToPairIso(eAB)
      val iso = arrayIso(innerIso)
      Some((arr, iso))
    case Def(view: ViewArray[a, b]) =>
      Some((view.source, view.iso))
    case UserTypeArray(iso: Iso[a, b]) =>
      val newIso = arrayIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[Array[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type UnpackedSeq[T] = (Seq[Rep[Source]], Iso[Source, T]) forSome { type Source }

  object HasEqualViews {
    def unapply[T](syms: Seq[Exp[T]]): Option[UnpackedSeq[T]] = {
      val sourceIsos = syms.map { l => l match {
        case HasViews(source, iso) => (source, iso)
        case s:Exp[a] => (s, identityIso[a](s.elem))
      }}
      val sources = sourceIsos.map(_._1)
      val isos = sourceIsos.map(_._2)
      isos.filterNot(_.asInstanceOf[Iso[_,_]].isIdentity).distinct.length == 1 match {
        case true => isos.head match {
          case iso: Iso[a,_] => Some((sources.map(_.asRep[a]), iso.asInstanceOf[Iso[a,T]]))
        }
        case _ => None
      }
    }
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    //------------------------------------------------------------
    // Iso lifting rules
    case SymsArray(HasEqualViews(sources: Seq[Rep[a]], iso: Iso[_,b])) =>
      val iso1 = iso.asInstanceOf[Iso[a,b]]
      implicit val eA = iso1.eFrom
      implicit val eB = iso1.eTo
      ArrayIso(iso1).to(SArray.fromSyms[a](sources)(iso1.eFrom))

    // Rule: V(arr, iso).length ==> arr.length
    case ArrayLength(HasViews(arr, Def(iso: ArrayIso[a, b]))) =>
      array_length(arr.asRep[Array[a]])

    // Rule: V(arr, iso)(i) ==> V(arr(i), iso.innerIso)
    case ArrayApply(HasViews(arr, Def(iso: ArrayIso[a, b])), i) =>
      implicit val eA = iso.innerIso.eFrom
      implicit val eB = iso.innerIso.eTo
      val res = iso.innerIso.to(arr.asRep[Array[a]](i))
      res

    // Rule: replicate(len, V(x, iso)) ==> V(replicate(len,x), ArrayIso(iso))
    case ArrayReplicate(len: Exp[Int], HasViews(source2, iso2: Iso[a, _])) =>
      implicit val eA = iso2.eFrom
      val s = ArrayReplicate(len, source2.asRep[a])
      val res = ViewArray(s, iso2)
      res

    case ArrayUpdate(HasViews(xs, Def(iso: ArrayIso[a, b])), index, value@HasViews(_, _)) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[Array[a]]
      val value1 = iso.innerIso.from(value.asRep[b])
      ViewArray(xs1.update(index, value1), iso.innerIso)

    case ArrayUpdateMany(HasViews(xs, Def(iso: ArrayIso[a, b])), is, vs@HasViews(_, Def(_: ArrayIso[_, _]))) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[Array[a]]
      val vs1 = iso.from(vs.asRep[Array[b]])
      ViewArray(xs1.updateMany(is, vs1), iso.innerIso)

    case ArrayAppend(HasViews(xs, Def(iso: ArrayIso[a, b])), value@HasViews(_, _)) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[Array[a]]
      val value1 = iso.innerIso.from(value.asRep[b])
      ViewArray(xs1 :+ value1, iso.innerIso)

    case ArrayCons(value@HasViews(_, _), HasViews(xs, Def(iso: ArrayIso[a, b]))) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[Array[a]]
      val value1 = iso.innerIso.from(value.asRep[b])
      ViewArray(value1 +: xs1, iso.innerIso)

    case ArrayReverse(HasViews(source, Def(iso: ArrayIso[a, b]))) =>
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eB = innerIso.eTo
      ViewArray(source.asRep[Array[a]].reverse, innerIso)

    case ArrayFlatten(HasViews(xss, Def(iso: ArrayIso[_,_]))) =>
      iso.innerIso match {
        case Def(iso: ArrayIso[a,b]) =>
          val innerIso = iso.innerIso
          implicit val eA = innerIso.eFrom
          implicit val eB = innerIso.eTo
          ViewArray(xss.asRep[Array[Array[a]]].flatten, innerIso)
        case _ => super.rewriteDef(d)
      }

    case ArrayMapReduce(
        xs: Arr[a] @unchecked,
        _map@Def(Lambda(_, _, _, HasViews(_, Def(pairIso: PairIso[k1,v1,k2,v2])))), _reduce) =>
      val map = _map.asRep[a => (k2, v2)]
      val reduce = _reduce.asRep[((v2, v2)) => v2]
      val keyIso = pairIso.iso1
      val valIso = pairIso.iso2
      implicit val ea = xs.elem.eItem
      val res = xs.mapReduce[k1, v1](
        x => pairIso.from(map(x)),
        (x: Rep[v1], y: Rep[v1]) =>
          valIso.from(reduce(Pair(valIso.to(x), valIso.to(y))))
      )(pairIso.eA1, pairIso.eA2)
      ViewMap[k1,v1, k2, v2](res)(keyIso, valIso)

    case ArrayMapReduce(HasViews(xs, Def(iso: ArrayIso[a, b])), map_, reduce) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[Array[a]]
      map_.elem.eRange match {
        case pe: PairElem[k,v] =>
          val eK = pe.eFst
          val eV = pe.eSnd
          val map = map_.asRep[b => (k,v)]
          xs1.mapReduceBy(iso.innerIso.toFun >> map, reduce)(eK, eV)
      }

    case MapToArray(HasViews(_map, Def(mapIso: MapIso[k1,v1,k2,v2]))) =>
      val map = _map.asRep[MMap[k1, v1]]
      implicit val ek1 = mapIso.eK1
      implicit val ev1 = mapIso.eV1
      implicit val ek2 = mapIso.eK2
      implicit val ev2 = mapIso.eV2
      map.toArray.map({ case Pair(k,v) => Pair(mapIso.iso1.to(k), mapIso.iso2.to(v)) })

    case sort @ ArraySortBy(HasViews(_xs, Def(iso: ArrayIso[a,b])), _by, _o: Ordering[o]) =>
      val xs = _xs.asRep[Array[a]]
      val by = _by.asRep[b => o]
      implicit val ea = xs.elem.eItem
      implicit val eo = sort.eO
      implicit val o = _o
      val sorted = xs.sortBy((x: Rep[a]) => by(iso.innerIso.to(x)))
      ViewArray(sorted, iso.innerIso)

    case ArrayFold(HasViews(xs_, Def(iso: ArrayIso[a,b])), init: Rep[s], f_) =>
      val f = f_.asRep[((s,b)) => s]
      val xs = xs_.asRep[Array[a]]
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eS = f.elem.eRange
      xs.fold[s](init, fun { p => f((p._1, innerIso.to(p._2))) })

    // Rule: ArrayFold(xs, V(init, iso), step) ==> iso.to(ArrayFold(xs, init, p => iso.from(step(iso.to(p._1), p._2)) ))
    case ArrayFold(xs: Arr[t], HasViews(init_, Def(iso: IsoUR[a, b])), step) =>
      val init = init_.asRep[a]
      implicit val eT = xs.elem.eItem
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val step1 = fun { (p: Rep[(a,t)]) =>
        val x_viewed = (iso.to(p._1), p._2)
        val res_viewed = step.asRep[((b,t)) => b](x_viewed)
        val res: Rep[a] = iso.from(res_viewed)
        res
      }
      val foldRes: Rep[a] = ArrayFold(xs, init, step1)
      iso.to(foldRes)

    case ArrayApplyMany(HasViews(xs, Def(iso: ArrayIso[a, b])), is) =>
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eB = innerIso.eTo
      ViewArray(xs.asRep[Array[a]](is), innerIso)

    case (_: ArrayFlatMap[_,r]) && ArrayFlatMap(HasViews(xs_, Def(iso: ArrayIso[a, b])), f_) =>
      val xs = xs_.asRep[Array[a]]
      val f = f_.asRep[b => Array[r]]
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eR = f.elem.eRange.eItem
      xs.flatMapBy(innerIso.toFun >> f)

    case ArrayFlatMap(xs: Arr[a] @unchecked, f@Def(Lambda(_, _, _, HasViews(_, Def(arrIso: ArrayIso[c, b]))))) =>
      val f1 = f.asRep[a => Array[b]]
      val xs1 = xs.asRep[Array[a]]
      implicit val eA = xs1.elem.eItem
      implicit val eC = arrIso.innerIso.eFrom
      val s = xs1.flatMapBy(arrIso.fromFun << f1)
      val res = ViewArray(s, arrIso.innerIso)
      res

    case (_: ArrayMap[_,r]) && ArrayMap(HasViews(xs_, Def(iso: ArrayIso[a, b])), f_) =>
      val xs = xs_.asRep[Array[a]]
      val f = f_.asRep[b => r]
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      implicit val eR = f.elem.eRange
      xs.mapBy(innerIso.toFun >> f)

    case ArrayMap(xs: Arr[a] @unchecked, f@Def(Lambda(_, _, _, HasViews(_, iso: Iso[c, b])))) =>
      val f1 = f.asRep[a => b]
      val xs1 = xs.asRep[Array[a]]
      implicit val eA = xs1.elem.eItem
      implicit val eC = iso.eFrom
      val s = xs1.mapBy(iso.fromFun << f1)
      val res = ViewArray(s, iso)
      res

    case red @ ArrayReduce(HasViews(xs_, Def(iso: ArrayIso[a,b])), _) =>
      val m = red.m.asInstanceOf[RepMonoid[b]]
      val xs = xs_.asRep[Array[a]]
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      val zeroA = innerIso.from(m.zero)
      val newMonoid = new RepMonoid[a](
        m.opName, zeroA,
        fun { p: Rep[(a, a)] => innerIso.from(m.append(innerIso.to(p._1), innerIso.to(p._2)))},
        m.isCommutative)
      val res: Rep[b] = innerIso.to(xs.reduce(newMonoid))
      res

    case ArrayFilter(HasViews(xs, Def(iso: ArrayIso[a, b])), pred_) =>
      implicit val eA = iso.innerIso.eFrom
      val pred = pred_.asRep[b => Boolean]
      val filtered = xs.asRep[Array[a]].filterBy(iso.innerIso.toFun >> pred)
      ViewArray(filtered, iso.innerIso)

    case pa @ ArrayZip(Def(v1:ViewArray[a,_]), arr2: Arr[b] @unchecked) =>
      implicit val eA = v1.source.elem.eItem
      implicit val eB = arr2.elem.eItem
      val iso2 = identityIso(eB)
      val pairIso1 = pairIso(v1.innerIso, iso2)
      val zipped = v1.source zip arr2
      ViewArray(zipped, pairIso1)

    case pa @ ArrayZip(arr1: Arr[a] @unchecked, Def(v2:ViewArray[_,_])) =>
      implicit val eA = arr1.elem.eItem
      val iso2 = identityIso(eA)
      val pairIso1 = pairIso(iso2, v2.innerIso)
      val zipped = arr1 zip v2.source
      ViewArray(zipped, pairIso1)

    case ArrayToList(Def(view: ViewArray[a, b])) =>
      val iso = view.innerIso
      implicit val eA: Elem[a] = iso.eFrom
      ViewList(view.source.toList, iso)
    case view1@ViewArray(Def(view2@ViewArray(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewArray(arr, compIso)
    case _ =>
      super.rewriteDef(d)
  }
}