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

  "isSymmetric" should "return true if left subtree mirrors right" in {
    Node('a', Node('b'), Node('c')).isSymmetric should be(true)
    End.isSymmetric should be(true)
    Node(
      'a',
      Node('b', End, Node('c')),
      Node('d', Node('e'), End)
    ).isSymmetric should be(true)
  }

  "isSymmetric" should "return whether BST is symmetric" in {
    S99Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric should be(true)
    S99Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric should not be (true)
  }

  "addValue" should "return tree with new value obeying BST property" in {
    End
      .addValue(3)
      .addValue(4)
      .addValue(0)
      .addValue(2)
      .addValue(1)
      .addValue(-1)
      .toString() should be(
      "T(3 T(0 T(-1 . .) T(2 T(1 . .) .)) T(4 . .))"
    )
  }

  "fromList" should "return BST with of list values" in {
    S99Tree.fromList(List(3, 2, 5, 7, 1)).toString() should be(
      "T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))"
    )
  }

  "symmetricBalancedTrees" should "return all balanced symmetric trees with n elements" in {
    S99Tree.symmetricBalancedTrees(5, "x").map(_.toString()) should be(
      List(
        "T(x T(x . T(x . .)) T(x T(x . .) .))",
        "T(x T(x T(x . .) .) T(x . T(x . .)))"
      )
    )
  }
}
