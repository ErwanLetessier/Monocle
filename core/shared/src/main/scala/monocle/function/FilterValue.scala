package monocle.function

import monocle.{Iso, Traversal}

import scala.annotation.implicitNotFound
import scala.collection.immutable.SortedMap
import cats.syntax.traverse._
import cats.{Applicative, Order, Traverse}

/**
 * Typeclass that defines a [[Traversal]] from an `S` to all its elements `A` that satisfy the predicate
 * @tparam S source of [[Traversal]]
 * @tparam A target of [[Traversal]]
 */
@implicitNotFound("Could not find an instance of FilterValue[${S},${A}], please check Monocle instance location policy to " +
  "find out which import is necessary")
abstract class FilterValue[S, A] extends Serializable {
  def filterValue(predicate: A => Boolean): Traversal[S, A]
}

trait FilterValueFunctions {
  def filterValue[S, A](predicate: A => Boolean)(implicit ev: FilterValue[S, A]): Traversal[S, A] = ev.filterValue(predicate)

}

object FilterValue extends FilterValueFunctions with FilterValueInstancesScalaVersionSpecific {
  /** lift an instance of [[FilterValue]] using an [[Iso]] */
  def fromIso[S, A, B](iso: Iso[S, A])(implicit ev: FilterValue[A, B]): FilterValue[S, B] = new FilterValue[S, B] {
    def filterValue(predicate: B => Boolean): Traversal[S, B] =
      iso composeTraversal ev.filterValue(predicate)
  }

  def fromTraverse[S[_]: Traverse, A]: FilterValue[S[A], A] = new FilterValue[S[A], A]{
    def filterValue(predicate: A => Boolean) = new Traversal[S[A], A] {
      def modifyF[F[_]: Applicative](f: A => F[A])(s: S[A]): F[S[A]] =
        s.traverse { case a => if(predicate(a)) f(a) else Applicative[F].pure(a) }
    }
  }

  /************************************************************************************************/
  /** Std instances                                                                               */
  /************************************************************************************************/
  import cats.instances.list._
  import cats.instances.vector._

  implicit def listFilterValue[A]: FilterValue[List[A], A] = fromTraverse

  implicit def sortedMapFilterValue[K, V](implicit ok: Order[K]): FilterValue[SortedMap[K,V], V] = new FilterValue[SortedMap[K, V], V] {
    import cats.syntax.applicative._
    import cats.syntax.functor._

    def filterValue(predicate: V => Boolean) = new Traversal[SortedMap[K, V], V] {
      def modifyF[F[_]: Applicative](f: V => F[V])(s: SortedMap[K, V]): F[SortedMap[K, V]] =
        s.toList.traverse{ case (k, v) =>
          (if(predicate(v)) f(v) else v.pure[F]).tupleLeft(k)
        }.map(kvs => SortedMap(kvs: _*)(ok.toOrdering))
    }
  }

  implicit val stringFilterValue: FilterValue[String, Char] = new FilterValue[String, Char]{
    def filterValue(predicate: Char => Boolean) =
      monocle.std.string.stringToList composeTraversal FilterValue.filterValue[List[Char], Char](predicate)
  }

  implicit def vectorFilterValue[A]: FilterValue[Vector[A], A] = fromTraverse

  /************************************************************************************************/
  /** Cats instances                                                                            */
  /************************************************************************************************/
  import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}

  implicit def chainFilterValue[A]: FilterValue[Chain[A], A] = fromTraverse

  implicit def necFilterValue[A]: FilterValue[NonEmptyChain[A], A] = fromTraverse

  implicit def nelFilterValue[A]: FilterValue[NonEmptyList[A], A] = fromTraverse

  implicit def nevFilterValue[A]: FilterValue[NonEmptyVector[A], A] = fromTraverse

}
