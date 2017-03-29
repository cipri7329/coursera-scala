import patmat.Huffman.{Fork, Leaf, weight}
import patmat._

val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

val w1 = weight(t1)
println(w1)

t2.chars

val mapChars = Huffman.times(t2.chars)

val charList1 = List('a', 'a', 'd', 'e', 'e', 'a', 'f', 'g', 'h', 'a', 'i', 'e')
val times1 = Huffman.times(charList1)





