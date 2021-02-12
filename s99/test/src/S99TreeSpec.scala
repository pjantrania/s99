import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import s99.binarytree.S99Tree

import s99.binarytree.{Node, End}

class S99TreeSpec extends AnyFlatSpec with should.Matchers {
  "cBalanced" should "return all possible balanced binary trees of size n" in {
    S99Tree.cBalanced(4, "x") should contain theSameElementsAs (
      List(
        Node(
          value = "x",
          left = Node(value = "x", left = End, right = End),
          right = Node(
            value = "x",
            left = End,
            right = Node(value = "x", left = End, right = End)
          )
        ),
        Node(
          value = "x",
          left = Node(
            value = "x",
            left = End,
            right = Node(value = "x", left = End, right = End)
          ),
          right = Node(value = "x", left = End, right = End)
        ),
        Node(
          value = "x",
          left = Node(value = "x", left = End, right = End),
          right = Node(
            value = "x",
            left = Node(value = "x", left = End, right = End),
            right = End
          )
        ),
        Node(
          value = "x",
          left = Node(
            value = "x",
            left = Node(value = "x", left = End, right = End),
            right = End
          ),
          right = Node(value = "x", left = End, right = End)
        )
      )
    )
  }
}
