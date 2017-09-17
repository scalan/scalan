package scalanizer.collections {
  import scalan._

  import scala.reflect.runtime.universe._

  import scala.reflect._

  package impl {
    trait ColsDefs extends scalan.Scalan with Cols { self: ColsModule =>
      implicit def proxyCol[A](p: Rep[Col[A]]): Col[A] = proxyOps[Col[A]](p)(scala.reflect.classTag[Col[A]]);
      class ColElem[A, To <: Col[A]](implicit _eA: Elem[A]) extends EntityElem[To] {
        def eA = _eA;
        lazy val parent: Option[(Elem[_$1] forSome { 
          type _$1
        })] = None;
        lazy val typeArgs = TypeArgs("A".->(eA.->(scalan.util.Invariant)));
        override lazy val tag = {
          implicit val tagA = eA.tag;
          weakTypeTag[Col[A]].asInstanceOf[WeakTypeTag[To]]
        };
        override def convert(x: Rep[(Def[_$2] forSome { 
          type _$2
        })]) = {
          implicit val eTo: Elem[To] = this;
          val conv = fun(((x: Rep[Col[A]]) => convertCol(x)));
          tryConvert(element[Col[A]], this, x, conv)
        };
        def convertCol(x: Rep[Col[A]]): Rep[To] = x.elem match {
          case (e @ ((_): ColElem[(_), (_)])) => x.asRep[To]
          case (e @ _) => !!!(StringContext("Expected ", " to have ColElem[_, _], but got ", "").s(x, e), x)
        };
        override def getDefaultRep: Rep[To] = ???
      };
      implicit def colElement[A](implicit eA: Elem[A]): Elem[Col[A]] = cachedElem[ColElem[A, Col[A]]](eA);
      implicit case object ColCompanionElem extends CompanionElem[ColCompanionCtor] with scala.Product with scala.Serializable {
        lazy val tag = weakTypeTag[ColCompanionCtor];
        protected def getDefaultRep = Col
      };
      abstract class ColCompanionCtor extends CompanionDef[ColCompanionCtor] with ColCompanion {
        def selfType = ColCompanionElem;
        override def toString = "Col"
      };
      implicit def proxyColCompanionCtor(p: Rep[ColCompanionCtor]): ColCompanionCtor = proxyOps[ColCompanionCtor](p);
      case class ColOverArrayCtor[A](override val arr: Rep[WArray[A]])(implicit eA: Elem[A]) extends ColOverArray[A](arr) with Def[ColOverArray[A]] {
        lazy val selfType = element[ColOverArray[A]]
      };
      class ColOverArrayElem[A](val iso: Iso[ColOverArrayData[A], ColOverArray[A]])(implicit override val eA: Elem[A]) extends ColElem[A, ColOverArray[A]] with ConcreteElem[ColOverArrayData[A], ColOverArray[A]] {
        override lazy val parent: Option[(Elem[_$3] forSome { 
          type _$3
        })] = Some(colElement(element[A]));
        override lazy val typeArgs = TypeArgs("A".->(eA.->(scalan.util.Invariant)));
        override def convertCol(x: Rep[Col[A]]) = ColOverArray(x.arr);
        override def getDefaultRep = ColOverArray(element[WArray[A]].defaultRepValue);
        override lazy val tag = {
          implicit val tagA = eA.tag;
          weakTypeTag[ColOverArray[A]]
        }
      };
      type ColOverArrayData[A] = WArray[A];
      class ColOverArrayIso[A](implicit eA: Elem[A]) extends EntityIso[ColOverArrayData[A], ColOverArray[A]] with Def[ColOverArrayIso[A]] {
        override def from(p: Rep[ColOverArray[A]]) = p.arr;
        override def to(p: Rep[WArray[A]]) = {
          val arr = p;
          ColOverArray(arr)
        };
        lazy val eFrom = element[WArray[A]];
        lazy val eTo = new ColOverArrayElem[A](self);
        lazy val selfType = new ColOverArrayIsoElem[A](eA);
        def productArity = 1;
        def productElement(n: Int) = eA
      };
      case class ColOverArrayIsoElem[A](eA: Elem[A]) extends Elem[ColOverArrayIso[A]] {
        def getDefaultRep = reifyObject(new ColOverArrayIso[A]()(eA));
        lazy val tag = {
          implicit val tagA = eA.tag;
          weakTypeTag[ColOverArrayIso[A]]
        };
        lazy val typeArgs = TypeArgs("A".->(eA.->(scalan.util.Invariant)))
      };
      class ColOverArrayCompanionCtor extends CompanionDef[ColOverArrayCompanionCtor] with ColOverArrayCompanion {
        def selfType = ColOverArrayCompanionElem;
        override def toString = "ColOverArrayCompanion";
        @scalan.OverloadId("fromFields") def apply[A](arr: Rep[WArray[A]])(implicit eA: Elem[A]): Rep[ColOverArray[A]] = mkColOverArray(arr);
        def unapply[A](p: Rep[Col[A]]) = unmkColOverArray(p)
      };
      lazy val ColOverArrayRep: Rep[ColOverArrayCompanionCtor] = new ColOverArrayCompanionCtor();
      lazy val ColOverArray: ColOverArrayCompanionCtor = proxyColOverArrayCompanion(ColOverArrayRep);
      implicit def proxyColOverArrayCompanion(p: Rep[ColOverArrayCompanionCtor]): ColOverArrayCompanionCtor = proxyOps[ColOverArrayCompanionCtor](p);
      implicit case object ColOverArrayCompanionElem extends CompanionElem[ColOverArrayCompanionCtor] with scala.Product with scala.Serializable {
        lazy val tag = weakTypeTag[ColOverArrayCompanionCtor];
        protected def getDefaultRep = ColOverArrayRep
      };
      implicit def proxyColOverArray[A](p: Rep[ColOverArray[A]]): ColOverArray[A] = proxyOps[ColOverArray[A]](p);
      implicit class ExtendedColOverArray[A](p: Rep[ColOverArray[A]])(implicit eA: Elem[A]) {
        def toData: Rep[ColOverArrayData[A]] = isoColOverArray(eA).from(p)
      };
      implicit def isoColOverArray[A](implicit eA: Elem[A]): Iso[ColOverArrayData[A], ColOverArray[A]] = reifyObject(new ColOverArrayIso[A]()(eA));
      registerModule(ColsModule);
      lazy val Col: Rep[ColCompanionCtor] = {
        final class $anon extends ColCompanionCtor;
        new $anon()
      };
      object ColOverArrayMethods {
        object length {
          def unapply(d: (Def[_$4] forSome { 
            type _$4
          })): Option[(Rep[ColOverArray[A]] forSome { 
            type A
          })] = d match {
            case MethodCall((receiver @ _), (method @ _), _, _) if receiver.elem.isInstanceOf[(ColOverArrayElem[_$5] forSome { 
  type _$5
})].&&(method.getName.==("length")) => Some(receiver).asInstanceOf[Option[(Rep[ColOverArray[A]] forSome { 
              type A
            })]]
            case _ => None
          };
          def unapply(exp: (Exp[_$6] forSome { 
            type _$6
          })): Option[(Rep[ColOverArray[A]] forSome { 
            type A
          })] = exp match {
            case Def((d @ _)) => unapply(d)
            case _ => None
          }
        };
        object apply {
          def unapply(d: (Def[_$7] forSome { 
            type _$7
          })): Option[(scala.Tuple2[Rep[ColOverArray[A]], Rep[Int]] forSome { 
            type A
          })] = d match {
            case MethodCall((receiver @ _), (method @ _), Seq((i @ _), _*), _) if receiver.elem.isInstanceOf[(ColOverArrayElem[_$8] forSome { 
  type _$8
})].&&(method.getName.==("apply")) => Some(scala.Tuple2(receiver, i)).asInstanceOf[Option[(scala.Tuple2[Rep[ColOverArray[A]], Rep[Int]] forSome { 
              type A
            })]]
            case _ => None
          };
          def unapply(exp: (Exp[_$9] forSome { 
            type _$9
          })): Option[(scala.Tuple2[Rep[ColOverArray[A]], Rep[Int]] forSome { 
            type A
          })] = exp match {
            case Def((d @ _)) => unapply(d)
            case _ => None
          }
        }
      };
      object ColOverArrayCompanionMethods;
      def mkColOverArray[A](arr: Rep[WArray[A]])(implicit eA: Elem[A]): Rep[ColOverArray[A]] = new ColOverArrayCtor[A](arr);
      def unmkColOverArray[A](p: Rep[Col[A]]) = p.elem.asInstanceOf[(Elem[_$10] forSome { 
        type _$10
      })] match {
        case ((_): ColOverArrayElem[A] @unchecked) => Some(p.asRep[ColOverArray[A]].arr)
        case _ => None
      };
      object ColMethods {
        object arr {
          def unapply(d: (Def[_$11] forSome { 
            type _$11
          })): Option[(Rep[Col[A]] forSome { 
            type A
          })] = d match {
            case MethodCall((receiver @ _), (method @ _), _, _) if receiver.elem.isInstanceOf[(ColElem[_$12, _$13] forSome { 
  type _$12;
  type _$13
})].&&(method.getName.==("arr")) => Some(receiver).asInstanceOf[Option[(Rep[Col[A]] forSome { 
              type A
            })]]
            case _ => None
          };
          def unapply(exp: (Exp[_$14] forSome { 
            type _$14
          })): Option[(Rep[Col[A]] forSome { 
            type A
          })] = exp match {
            case Def((d @ _)) => unapply(d)
            case _ => None
          }
        };
        object length {
          def unapply(d: (Def[_$15] forSome { 
            type _$15
          })): Option[(Rep[Col[A]] forSome { 
            type A
          })] = d match {
            case MethodCall((receiver @ _), (method @ _), _, _) if receiver.elem.isInstanceOf[(ColElem[_$16, _$17] forSome { 
  type _$16;
  type _$17
})].&&(method.getName.==("length")) => Some(receiver).asInstanceOf[Option[(Rep[Col[A]] forSome { 
              type A
            })]]
            case _ => None
          };
          def unapply(exp: (Exp[_$18] forSome { 
            type _$18
          })): Option[(Rep[Col[A]] forSome { 
            type A
          })] = exp match {
            case Def((d @ _)) => unapply(d)
            case _ => None
          }
        };
        object apply {
          def unapply(d: (Def[_$19] forSome { 
            type _$19
          })): Option[(scala.Tuple2[Rep[Col[A]], Rep[Int]] forSome { 
            type A
          })] = d match {
            case MethodCall((receiver @ _), (method @ _), Seq((i @ _), _*), _) if receiver.elem.isInstanceOf[(ColElem[_$20, _$21] forSome { 
  type _$20;
  type _$21
})].&&(method.getName.==("apply")) => Some(scala.Tuple2(receiver, i)).asInstanceOf[Option[(scala.Tuple2[Rep[Col[A]], Rep[Int]] forSome { 
              type A
            })]]
            case _ => None
          };
          def unapply(exp: (Exp[_$22] forSome { 
            type _$22
          })): Option[(scala.Tuple2[Rep[Col[A]], Rep[Int]] forSome { 
            type A
          })] = exp match {
            case Def((d @ _)) => unapply(d)
            case _ => None
          }
        }
      };
      object ColCompanionMethods {
        object fromArray {
          def unapply(d: (Def[_$23] forSome { 
            type _$23
          })): Option[(scala.Tuple2[Rep[WArray[T]], Elem[T]] forSome { 
            type T
          })] = d match {
            case MethodCall((receiver @ _), (method @ _), Seq((arr @ _), (emT @ _), _*), _) if receiver.elem.==(ColCompanionElem).&&(method.getName.==("fromArray")) => Some(scala.Tuple2(arr, emT)).asInstanceOf[Option[(scala.Tuple2[Rep[WArray[T]], Elem[T]] forSome { 
              type T
            })]]
            case _ => None
          };
          def unapply(exp: (Exp[_$24] forSome { 
            type _$24
          })): Option[(scala.Tuple2[Rep[WArray[T]], Elem[T]] forSome { 
            type T
          })] = exp match {
            case Def((d @ _)) => unapply(d)
            case _ => None
          }
        };
        object ddmvm {
          def unapply(d: (Def[_$25] forSome { 
            type _$25
          })): Option[Rep[WArray[Double]]] = d match {
            case MethodCall((receiver @ _), (method @ _), Seq((v @ _), _*), _) if receiver.elem.==(ColCompanionElem).&&(method.getName.==("ddmvm")) => Some(v).asInstanceOf[Option[Rep[WArray[Double]]]]
            case _ => None
          };
          def unapply(exp: (Exp[_$26] forSome { 
            type _$26
          })): Option[Rep[WArray[Double]]] = exp match {
            case Def((d @ _)) => unapply(d)
            case _ => None
          }
        }
      }
    }

    object ColsModule extends scalan.ModuleInfo {
      val dump = "H4sIAAAAAAAAALVWXWgcRRyf27vkvkJoog1Rq6bpBT97VyzSYpBwTS6Sck1CtzV4FmVud3LdOrs7zs6ld1LqWx/0TcQH0YeC0pegiC9FREQriEgffBMfpSCIIn2wKFj8z+zH7V1v0/jgPgwzs//5f/x+v/nvbv+GRjyOZjwDU+yUbSJwWVfzqidK+gnXbFOyRDbf+8s4v547mtfQZAONnsXekkcbKO9Pah0WzXVh1tHEsuWYNUdYoluylQuBynU/RkXGqAyLUYqdmq+jabnc4Jgxwgd8Pb07X/2HwWUeOwbxhMs9gfb7PiqGSykxhOU6Fcu22wI3KanULU+Afabpmt1X0UWUrqM9husYnAiiL1LsecQL9nNEureidV6tu2usF+POPE9xbAlIE2Ls8e1PEqZ3Hdfp2gKNB6mtMZkW2BRJhwG2KzajKsxIHWUtm7lchFGzEOGsa4bLjINhA03Wz+EtXIGorYouuOW0pDOGjVdwi6yCiTQfhRo8QjdPdRkJnBc9YfbF6zCEEGOglKdUauUeauUItbJEraQTbmFqvYbly3XudrrIf1JphDrSxZN3cRF6IDXHLL1xxnjxll60NXm4I5PJq5Ry4OjhBNUqggDdb0++5d187vIRDRUaqGB51aYnODZEXAgBYEXsOK5QOUcYYt4CDmeTOFRRqmAzIJS84doMO+ApQHMMqKKWYQlpLPfGA4IS0M8KRkLTNCAf1Zt0S+XZKmO0+/WFLy78vO/HCc0XZofxmNs0uN2hHCXJRUwplKOJMDhELfh06a5NJmZvWi9dflNoKFVHqU6/wNaa54DO+Q5HY/4JX763rSP//DS+KbSA/cQiwvifZ7/86pcbCxkNaf045aEAHXoND5MTKL3o0gAeOU4LlKoqjcihqORy38A6v0MOEaWP/Pq7+c0hdEYVqoQQ4rEr7YGLyaPvfjZH1j/SUK6hLusyxS2lQknJEvGMBsq5W4T7+9ktTOVsqBKzJtnEbSoCIuOY+KTOJJLKiARsXt3fVFh+0edn1XVIaXm99Kf+3dvbkh75fgpAxZyHcI5uVDnH3btgXIxdzHt3ACdse59curT3jw9evkddzFzTEjZmpUP/4VqGt+j/vHZRjX5dD/XWcpgD9e0F9a0BhQqixXj4udi5GHb7UynU8zsnkEaqIaiZGiW2byuHfXF0BRqLh1KHIyU+mAS2Kufjv49dOXD/A7c1lD2ORjZBYt5QWEeabtsxQ+jgWydIRxwL99L90AFUmGM7+gRuYejYQJ1AU6ES28KileeDfV9/8MzEqlezSExQyFRQiDxaXnF8p6L0xNXt89b1x5ZVA4mjEgP2IIotJgIdBsCmQXD9+t1F1/DH2h29Y0AJsUgHdyEYyeIQnaiqVuKsyvGZBMjkUI9mC4M5V3t+5FUsJ6hjiRgUc2LKbzyx4R/EV8PhdxY2jk9vnFaNb8xURv6bqMsM/2M6gdm86u+P7vB1B6NSzWaiKyeHrz37w+vfX/kw4jUfKCIj6RGhHOA/gMe8eVFlswmV6cG1Btov3np/9fHrn95Qn5KCbBDQ8pzop6kn6aixB3ooyBT8n8mYBCA12TNipJ+Wwwv/AuCg2YtFCwAA"
    }
  }
}