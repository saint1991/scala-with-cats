package com.github.saint1991.cats.chapter3

import com.github.saint1991.cats.chapter3.Exercise1.{Branch, Leaf, Tree}
import org.scalatest.{Matchers, WordSpec}

class FunctorTest extends WordSpec with Matchers {

  {

    import cats.syntax.functor._
    import Exercise1.treeFunctor

    "TreeFunctor should map all leaf values" in {
      val tree: Tree[Int] = Branch(
        Branch(
          Leaf(1), Leaf(2)
        ),
        Branch(
          Branch(
            Branch(
              Leaf(3), Leaf(4)
            ),
            Leaf(5)
          ),
          Leaf(6)
        )
      )

      val actual = tree.map { x => x * 2 }
      actual should equal (Branch(
        Branch(
          Leaf(2), Leaf(4)
        ),
        Branch(
          Branch(
            Branch(
              Leaf(6), Leaf(8)
            ),
            Leaf(10)
          ),
          Leaf(12)
        )
      ))
    }
  }


}
