package patmat

import org.scalatest.{FunSuite, Ignore}
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







  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode with quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("ab".toList)) === "ab".toList)
    }
  }

  test ("decode and encode with quickEncode a text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abbbb".toList)) === "abbbb".toList)
    }
  }

  test ("decode and encode with quickEncode a text abbaa should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abbaa".toList)) === "abbaa".toList)
    }
  }

}


@RunWith(classOf[JUnitRunner])
class HuffmanUntilTreeSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Leaf('d', 1), Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2), List('d', 'a', 'b'), 3)

    val charList2 = List('d', 'b', 'a')
    val charList1 = List('a', 'a', 'd', 'e', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')

    val charList3 = List('x', 'y', 'd', '_', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')
    val charList4 = List('x', 'y', 'd', '_', 'e', 'a', 'f', 'g', 'h', '1', '2', '3')
    val charList5 = List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a')
  }

  test("until some leaf list") {
    val leafList1 = List(Leaf('g', 2), Leaf('h', 2), Leaf('i', 3), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3))
    val leafList2 = List(Leaf('i', 3), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Fork(Leaf('g', 2), Leaf('h', 2), List('g', 'h'), 4))
    val leafList3 = List(
      Fork(Leaf('g', 2), Leaf('h', 2), List('g', 'h'), 4),
      Fork(Leaf('i', 3), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), List('i', 'e', 't'), 6))

    val leafList4 = List( Leaf('d', 1),  Leaf('f', 1), Leaf('g', 1), Leaf('h', 1), Leaf('i', 1), Leaf('e', 3), Leaf('a',4))

    until(singleton, combine)(leafList4)
  }

}



@RunWith(classOf[JUnitRunner])
class HuffmanCombineTreeSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Leaf('d', 1), Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2), List('d', 'a', 'b'), 3)

    val charList2 = List('d', 'b', 'a')
    val charList1 = List('a', 'a', 'd', 'e', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')

    val charList3 = List('x', 'y', 'd', '_', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')
    val charList4 = List('x', 'y', 'd', '_', 'e', 'a', 'f', 'g', 'h', '1', '2', '3')
    val charList5 = List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a')
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3), Leaf('x',4)))
  }


  test("combine of some leaf list - order change") {
    val leaflist = List(Leaf('g', 2), Leaf('h', 2), Leaf('i', 3), Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3))
    assert(combine(leaflist) === List(Leaf('i',3), Fork(Leaf('e',1),Leaf('t',2), List('e', 't'),3), Fork(Leaf('g',2),Leaf('h',2), List('g', 'h'),4)))
  }


}


@RunWith(classOf[JUnitRunner])
class HuffmanCreateCodeTreeSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Leaf('d',1), Fork(Leaf('a',1), Leaf('b',1), List('a','b'), 2),  List('d','a','b'), 3)

    val charList2 = List('d', 'b', 'a')
    val charList1 = List('a', 'a', 'd', 'e', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')

    val charList3 = List('x', 'y', 'd', '_', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')
    val charList4 = List('x', 'y', 'd', '_', 'e', 'a', 'f', 'g', 'h', '1', '2', '3')
    val charList5 = List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a')
  }


  test("createCodeTree - charList2") {
    new TestTrees {
      assert(createCodeTree(charList2) === t2)
    }
  }



  test("createCodeTree - charList1") {
    new TestTrees {
      assert(createCodeTree(charList1) === t2)
    }
  }

  ignore("createCodeTree - charList5") {
    new TestTrees {
      assert(createCodeTree(charList5) === Leaf('a', 12))
    }
  }



}


@RunWith(classOf[JUnitRunner])
class HuffmanTimesSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val charList1 = List('a', 'a', 'd', 'e', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')
    val charList5 = List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a')
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

  test("makeOrderedLeafList - charList1") {
    new TestTrees {
      assert(makeOrderedLeafList(times(charList1)) === List( Leaf('d', 1),  Leaf('f', 1), Leaf('g', 1), Leaf('h', 1), Leaf('i', 1), Leaf('e', 3), Leaf('a',4)))
    }
  }

  test("makeOrderedLeafList - charList5") {
    new TestTrees {
      assert(makeOrderedLeafList(times(charList5)) === List( Leaf('a', 12)))
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