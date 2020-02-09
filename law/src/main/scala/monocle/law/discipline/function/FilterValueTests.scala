package monocle.law.discipline.function

import monocle.function.FilterValue._
import monocle.function._
import monocle.law.discipline.TraversalTests
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import cats.Eq

object FilterValueTests extends Laws {

  def apply[S: Eq : Arbitrary, A: Eq : Arbitrary](implicit evFilterValue: FilterValue[S, A],
                                                     arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet("FilterValue", TraversalTests(filterValue(_: A => Boolean)(evFilterValue)).props: _*)

}
