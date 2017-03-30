package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree - t1") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger tree - t2") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }


  test("chars of a larger tree - t1") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of a larger tree - t2") {
    new TestTrees {
      assert(chars(t1) === List('a','b'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }



  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3), Leaf('x',4)))
  }


  test("combine of some leaf list - order change") {
    val leaflist = List(Leaf('g', 2), Leaf('h', 2), Leaf('i', 3), Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3))
    assert(combine(leaflist) === List(Leaf('i',3), Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3), Fork(Leaf('g',2),Leaf('h',2), List('g', 'h'),4)))
  }


  test("until some leaf list") {
    val leaflist = List(Leaf('g', 2), Leaf('h', 2), Leaf('i', 3), Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3))
    val leafList2 = List(Leaf('i',3), Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3), Fork(Leaf('g',2),Leaf('h',2), List('g', 'h'),4))
    val leafList3 = List(
      Fork(Leaf('g',2),Leaf('h',2), List('g', 'h'),4),
      Fork(Leaf('i',3), Fork(Leaf('e',1), Leaf('t',2), List('e', 't'),3), List('i', 'e', 't'), 6))

  }



  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}


@RunWith(classOf[JUnitRunner])
class HuffmanTimesSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val charList1 = List('a', 'a', 'd', 'e', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')
  }


  test("times larger tree - t1") {
    new TestTrees {
      assert(times(t1.chars) === List(('a',1), ('b', 1)))
    }
  }


  test("times larger tree - charList1") {
    new TestTrees {
      assert(times(charList1) === List(('a',4), ('d', 1), ('e', 3), ('f', 1), ('g', 1), ('h', 1), ('i', 1)))
    }
  }

}



@RunWith(classOf[JUnitRunner])
class HuffmanDecodeSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val charList1 = List('a', 'a', 'd', 'e', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')
  }


  test("decode t1") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === List('a','b'))
    }
  }


  test("decode t2") {
    new TestTrees {
      assert(decode(t2, List(0, 0, 1, 0, 1)) === List('a','d','b'))
    }
  }


}