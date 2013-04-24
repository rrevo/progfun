package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
    assert(weight(Leaf('x', 9)) === 9)
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
    assert(chars(Leaf('x', 9)) === List('x'))
  }

  test("make tree") {
    new TestTrees {
      val t = makeCodeTree(makeCodeTree(Leaf('a', 2), Leaf('b', 1)), Leaf('c', 1))
      assert(weight(t) === 4)
      assert(chars(t) === List('a', 'b', 'c'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") ===
      List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    val t = times(string2Chars("allo"))
    assert(t.length === 3)
    assert(t.find(p => p._1 == 'a' && p._2 == 1).isEmpty === false)
    assert(t.find(p => p._1 == 'l' && p._2 == 2).isEmpty === false)
    assert(t.find(p => p._1 == 'o' && p._2 == 1).isEmpty === false)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) ===
      List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton") {
    assert(singleton(List(Leaf('1', 1))) == true)
    assert(singleton(Nil) == false)
    assert(singleton(List(Leaf('1', 1), Leaf('2', 2))) == false)
  }

  test("combine of some leaf list") {
    assert(combine(List()) == List())
    assert(combine(List(Leaf('a', 1))) == List(Leaf('a', 1)))

    val xLeaf = Leaf('x', 4)
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), xLeaf)
    val c1 = combine(leaflist)
    val etFork = Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5)
    assert(c1 === List(Leaf('x', 4), etFork))

    val c2 = combine(c1)
    assert(c2 === List(Fork(xLeaf, etFork, List('x', 'e', 't'), 9)))
  }

  test("createCodeTree test") {
    val tree = createCodeTree("text".toList)
    val head = tree.asInstanceOf[Fork]
    assert(head.chars == List('e', 'x', 't'))
    assert(head.weight == 4)

    val xeTree = head.left.asInstanceOf[Fork]
    assert(xeTree.chars == List('e', 'x'))
    assert(xeTree.weight == 2)

    val eLeaf = xeTree.left.asInstanceOf[Leaf]
    assert(eLeaf.char == 'e')
    assert(eLeaf.weight == 1)

    val xLeaf = xeTree.right.asInstanceOf[Leaf]
    assert(xLeaf.char == 'x')
    assert(xLeaf.weight == 1)

    val tLeaf = head.right.asInstanceOf[Leaf]
    assert(tLeaf.char == 't')
    assert(tLeaf.weight == 2)
  }

  test("decode") {
    new TestTrees {
      assert(decode(t2, List(0, 0, 0, 1, 1)) == List('a', 'b', 'd'))
      assert(decode(Leaf('a', 1), List(0, 1, 1)) == List('a', 'a', 'a'))
    }
  }

  test("encode") {
    new TestTrees {
      assert(encode(t2)("bda".toList) == List(0, 1, 1, 0, 0))
      assert(encode(Leaf('a', 1))(List('a', 'a')) == List(0, 0))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quick") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
