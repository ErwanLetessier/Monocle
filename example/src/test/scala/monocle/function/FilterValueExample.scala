package monocle.function


import monocle.MonocleSuite
import monocle.std.StringOptics

import scala.collection.immutable.SortedMap

class FilterValueExample extends MonocleSuite with StringOptics  {

  test("filterValue creates Traversal from a SortedMap, IMap to all values matching the predicate") {

    (SortedMap(1 -> "One", 2 -> "Two") applyTraversal filterValue{v: String => v.toLowerCase.contains("o")} getAll) shouldEqual List("One", "Two")
    (SortedMap(1 -> "One", 2 -> "Two") applyTraversal filterValue{v: String => v.startsWith("T")} set "Three") shouldEqual SortedMap(1 -> "One", 2 ->"Three")

  }

  test("filterValue creates Traversal from a List, IList, Vector or Stream to all values matching the predicate") {

    (List(1,3,5,7,9) applyTraversal filterValue{ v: Int => v%3 == 0 } getAll) shouldEqual List(3,9)

    (List(1,3,5,7)   applyTraversal filterValue{ v: Int => v >= 4 } modify(_ + 2)) shouldEqual List(1,3,7,9)
    (Vector(1,3,5,7) applyTraversal filterValue{ v: Int => v >= 4 } modify(_ + 2)) shouldEqual Vector(1,3,7,9)
  }

  test("filterValue creates Traversal from a String all characters matching the predicate") {

    ("abcdef" applyTraversal filterValue { c: Char => c >= 'd'} getAll) shouldEqual List('d','e','f')

    (("abcdef" applyTraversal filterValue { c: Char => c >= 'd'} getAll) applyIso stringToList.reverse).get shouldEqual "def"
  }

}
