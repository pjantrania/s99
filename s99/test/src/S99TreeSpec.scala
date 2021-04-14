import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

import s99.binarytree.{S99Tree, Node, End}

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

  "hbalTrees" should "return all trees whose subtrees have same height in left and right up to difference = 1" in {
    val r = S99Tree.hbalTrees(4, "x")
    r.foreach { t =>
      t.maxDepthDifference should be <= 1
    }

    r.length should be(315)
  }

  "minHbalNodes" should "return the smallest number of nodes for height h" in {
    S99Tree.minHbalNodes(3) should be(4)
    S99Tree.minHbalNodes(4) should be(7)
  }

  "maxHbalHeight" should "return maximum height achievable with n nodes" in {
    (4 to 6).foreach { S99Tree.maxHbalHeight(_) should be(3) }
    (7 to 11).foreach { S99Tree.maxHbalHeight(_) should be(4) }
    (12 to 19).foreach { S99Tree.maxHbalHeight(_) should be(5) }
  }

  "hbalTreesWithNodes" should "return all height balanced trees with n nodes" in {
    val r = S99Tree.hbalTreesWithNodes(4, "x")
    r should have length (4)
    all(r.map(_.nodeCount)) should be(4)
    all(r.map(_.maxDepthDifference)) should be <= 1
  }

  "leafCount" should "count leaves in tree" in {
    all(S99Tree.cBalanced(5, "").map(_.leafCount)) should (be(2) or be(3))
  }

  "leafList" should "return list of leaf values" in {
    Node('a',
      Node('b'),
      Node('c',
        Node('d'),
        Node('e')
      )
    ).leafList should be(List('b', 'd', 'e'))
  }

  "nodeCount" should "count nodes in tree" in {
    val r = S99Tree.hbalTreesWithNodes(4, "x").head
    r.nodeCount should be(4)
  }

  "depth" should "return the depth of the tree" in {
    val r = S99Tree.hbalTrees(5, 'x').head
    r.depth should be(5)
  }

  "maxDepthDifference" should "return the maximal difference in subtree depths" in {
    /** test case has depth difference 1 at root, 2 at left subtree
              x
          x       x
        x   .   x   x
       x              
    */
    val t = Node(value = "x",
      left=Node(value = "x",
        left = Node(value = "x",
          left = Node(value = "x"),
          right = End
        ),
        right = End
      ),
      right = S99Tree.cBalanced(3, "x").head)
    
    t.maxDepthDifference should be(2)
  }

  "internalList" should "return list of values from non-leaf nodes" in {
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList should be(List('a', 'c'))
  }
}
