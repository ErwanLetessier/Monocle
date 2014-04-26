package monocle

import monocle.syntax.traversal._
import monocle.function.Each._
import org.specs2.scalaz.Spec


class EachExample extends Spec {

  "Each can be used on Option" in {
    Option(3)            |->> each modify( _ + 1) shouldEqual Some(4)
    (None : Option[Int]) |->> each modify( _ + 1) shouldEqual None
  }

  "Each can be used on List" in {
    List(1, 2)    |->> each modify( _ + 1) shouldEqual List(2, 3)
    List(1, 2, 3) |->> each modify( _ + 1) shouldEqual List(2, 3, 4)
  }

  "Each can be used on Map to update all values" in {
    Map("One" -> 1, "Two" -> 2) |->> each modify( _ + 1) shouldEqual Map("One" -> 2, "Two" -> 3)
  }

  "Each can be used on tuple of same type" in {
    (1, 2)    |->> each modify( _ + 1) shouldEqual (2, 3)
    (1, 2, 3) |->> each modify( _ + 1) shouldEqual (2, 3, 4)
  }

  "Each can be used on Vector" in {
    Vector(1, 2)    |->> each modify( _ + 1) shouldEqual Vector(2, 3)
    Vector(1, 2, 3) |->> each modify( _ + 1) shouldEqual Vector(2, 3, 4)
  }

}
