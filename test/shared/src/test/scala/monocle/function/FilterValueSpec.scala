package monocle.function

import cats.Order
import monocle.MonocleSuite
import monocle.law.discipline.function.FilterValueTests

class FilterValueSpec extends MonocleSuite {

  implicit def mmapFilterIndex[K: Order, V]: FilterValue[MSorteMap[K, V], V] =
    FilterValue.fromIso(MSorteMap.toSortedMap)

  checkAll("fromIso", FilterValueTests[MSorteMap[Int, String], String])

}
