package scalan.collections

import scalan._

/**
  * Created by dkolmakov on 12/28/15.
  */
trait MapViews extends MapOps with Views { self: Scalan =>
}

trait MapViewsStd extends MapViews with MapOpsStd with ViewsDslStd { self: ScalanStd =>
}

trait MapViewsExp extends MapViews with MapOpsExp with ViewsDslExp { self: ScalanExp =>

  case class ViewMap[K1, V1, K2, V2](source: MM[K1, V1])(implicit iso1: Iso[K1, K2], iso2: Iso[V1, V2]) extends View2[K1, V1, K2, V2, MMap] {
    implicit val k1 = iso1.eFrom
    implicit val k2 = iso1.eTo
    implicit val v1 = iso2.eFrom
    implicit val v2 = iso2.eTo
    lazy val iso = MapIso(iso1, iso2)
//    def copy(source: MM[K1, V1]) = ViewMap(source)
    override def toString = s"ViewMap[${iso1.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewMap[_, _, _, _] => source == v.source && iso1.eTo == v.iso1.eTo && iso2.eTo == v.iso2.eTo
      case _ => false
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewMap[_,_,_,_]) => Some((view.source, view.iso))
    case _ =>  super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case VarMM(map) => map

    case MapFromArray(HasViews(source: Arr[(a1, a2)] @unchecked, Def(arrIso: ArrayIso[_,_]))) =>
      arrIso.innerIso match {
        case Def(pairIso: PairIso[_, _, b1, b2]) =>
          val iso1 = pairIso.iso1.asInstanceOf[Iso[a1, b1]]
          val iso2 = pairIso.iso2.asInstanceOf[Iso[a2, b2]]
          implicit val eA1 = iso1.eFrom
          implicit val eB1 = iso1.eTo
          implicit val eA2 = iso2.eFrom
          implicit val eB2 = iso2.eTo
          val mNew = MMap.fromArray(source)
          ViewMap(mNew)(iso1, iso2)
      }

    case MapApply(HasViews(sourceMap: Rep[MMap[k, v]] @unchecked, Def(mapIso: MapIso[_, _, _, v1])), key: Rep[k1]) => {
      implicit val eK = mapIso.iso1.eFrom.asElem[k]
      implicit val eV = mapIso.iso2.eFrom.asElem[v]
      (mapIso.iso1.isIdentity, mapIso.iso2.isIdentity) match {
        case (true, false) =>
          // View for values
          val key1 = key.asRep[k]
          val isoV = mapIso.iso2.asInstanceOf[Iso[v, v1]]
          val value = MapApply[k, v](sourceMap, key1) //sourceMap.apply(key1)
          isoV.to(value)
        case (false, true) =>
          // View for key
          val isoK = mapIso.iso1.asInstanceOf[Iso[k, k1]]
          val value = MapApply[k, v](sourceMap, isoK.from(key))// sourceMap.apply(iso.from(key))
          value
        case (false, false) =>
          // Views for both
          val isoK = mapIso.iso1.asInstanceOf[Iso[k, k1]]
          val isoV = mapIso.iso2.asInstanceOf[Iso[v, v1]]
          val value = MapApply[k, v](sourceMap, isoK.from(key)) // sourceMap.apply(iso.from(key))
          isoV.to(value)
        case _ => d
      }
    }
    case MapContains(HasViews(sourceMap: Rep[MMap[k,v]] @unchecked, Def(mapIso: MapIso[_, _, _, _])), key: Rep[k1]) => {
      implicit val eK = mapIso.iso1.eFrom.asElem[k]
      implicit val eV = mapIso.iso2.eFrom.asElem[v]
      (mapIso.iso1.isIdentity) match {
        case (false) =>
          val iso = mapIso.iso1.asInstanceOf[Iso[k,k1]]
          val value = MapContains[k,v](sourceMap, iso.from(key))
          value
        case _ => sourceMap.contains(key.asRep[k])
      }
    }
    case MapUnion(HasViews(sourceMap1: Rep[MMap[k,v]] @unchecked, Def(mapIso1: MapIso[_,_,k2,v2])), HasViews(sourceMap2, mapIso2)) if(mapIso1 == mapIso2)=> {
      implicit val eK = mapIso1.iso1.eFrom.asElem[k]
      implicit val eV = mapIso1.iso2.eFrom.asElem[v]
      val union = sourceMap1.union(sourceMap2.asRep[MMap[k,v]])
      mapIso1.asInstanceOf[MapIso[k,v,k2,v2]].to(union)
    }

    case MapUsingFunc(count, LambdaResultHasViews(f, Def(iso: PairIso[a1, a2, b1, b2]))) => {
      val f1 = f.asRep[Int=>(b1,b2)]
      implicit val eA1 = iso.iso1.eFrom
      implicit val eA2 = iso.iso2.eFrom
      implicit val eB1 = iso.iso1.eTo
      implicit val eB2 = iso.iso2.eTo
      implicit val eA1A2 = iso.eFrom
      val mmap = MMap.create(count, fun { x: Rep[Int] =>
        val tmp = f1(x)
        iso.from(tmp)
      })
      MapIso(iso.iso1, iso.iso2).to(mmap)
    }

    case MapValues(HasViews(sourceMap: Rep[MMap[k,v]] @unchecked, Def(mapIso: MapIso[_, _, _, v1]))) => {
      implicit val eK = mapIso.iso1.eFrom.asElem[k]
      implicit val eV = mapIso.iso2.eFrom.asElem[v]
      implicit val eV1 = mapIso.iso2.eFrom.asElem[v1]
      (mapIso.iso2.isIdentity) match {
        case (false) =>
          val iso = mapIso.iso2.asInstanceOf[Iso[v,v1]]
          val values = ArrayIso(iso).to(sourceMap.values)
          values
        case _ => sourceMap.values
      }
    }

    case MapKeys(HasViews(sourceMap: Rep[MMap[k,v]] @unchecked, Def(mapIso: MapIso[_, _, k1, _]))) => {
      implicit val eK = mapIso.iso1.eFrom.asElem[k]
      implicit val eV = mapIso.iso2.eFrom.asElem[v]
      implicit val eK1 = mapIso.iso1.eFrom.asElem[k1]
      (mapIso.iso1.isIdentity) match {
        case (false) =>
          val iso = mapIso.iso1.asInstanceOf[Iso[k,k1]]
          val keys = ArrayIso(iso).to(sourceMap.keys)
          keys
        case _ => sourceMap.keys
      }
    }

    case _ => super.rewriteDef(d)
  }
}