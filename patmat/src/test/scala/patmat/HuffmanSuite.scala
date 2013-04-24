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

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val c1 = combine(leaflist)
    assert(c1 === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    val c1Fork: Fork = c1.head.asInstanceOf[Fork]
    val c2 = combine(c1)
    assert(c2 === List(Fork(c1Fork, Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  ignore("createCodeTree test") {
    Console.println(createCodeTree("text".toList))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t2, List(0, 0, 0, 1, 1)) == List('a', 'b', 'd'))
      assert(decode(Leaf('a', 1), List(0, 1, 1)) == List('a', 'a', 'a'))
    }
  }

  test("encode") {
    new TestTrees {
      assert(encode(Leaf('a', 1))(List('a', 'a')) == List(0, 0))
      assert(encode(t2)("bda".toList) == List(0, 1, 1, 0, 0))
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
