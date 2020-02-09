package monocle.function

import cats.instances.lazyList._

trait FilterValueInstancesScalaVersionSpecific {
  /************************************************************************************************/
  /** 2.13 std instances                                                                          */
  /************************************************************************************************/
  implicit def lazyListFilterValue[A]: FilterValue[LazyList[A], A] = FilterValue.fromTraverse
}