val n = 10

val list1 = ((1 until n) map ( i => (1 until i) map (j => (i, j)))).flatten

val list2 = (1 until n) flatMap (i => (1 until i) map (j => (i, j)))


val list3 = for {
  i <- 1 until n
  j <- 1 until i
} yield (i, j)



val xs1 = List(1, 2.0, 3, 4)
val ys1 = List(11, 22.0, 33, 44, 55)


def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
  (for {
    (x,y) <- xs zip ys
    _ = println(x + " " + y)
  } yield x*y).sum

val scal1 = scalarProduct(xs1, ys1)




/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]


/** `Occurrences` is a `List` of pairs of characters and positive integers saying
  *  how often the character appears.
  *  This list is sorted alphabetically w.r.t. to the character in each pair.
  *  All characters in the occurrence list are lowercase.
  *
  *  Any list of pairs of lowercase characters and their frequency which is not sorted
  *  is **not** an occurrence list.
  *
  *  Note: If the frequency of some character is zero, then that character should not be
  *  in the list.
  */
type Occurrences = List[(Char, Int)]


val val1 = "Aabcdabde"

val g2 = val1.groupBy(_.toChar)

val chars1 = val1.toList

val group1 = val1.toList.groupBy(i=>i).mapValues(_.size)


val1.groupBy( _ : Char => _).foreach(println)

val charOccurence = val1.toList.groupBy(i=>i.toLower).mapValues(_.size)

charOccurence.toList.sortBy{ case(key, value) => key}



def wordOccurrences(w: Word): Occurrences = w.toList.groupBy(i=>i.toLower).mapValues(_.size).toList.sortBy{ case(key, value) => key}


val sentence1 = List("Ana", "are", "mere")
val g3 = sentence1.flatMap(word => wordOccurrences(word))
  .groupBy{case (key, value) => key}

val g4 = g3.mapValues(_.reduce( (a, b) => (a._1, a._2 + b._2)))

val g5 = g4.mapValues{ case (_: Char, b: Int) => b}

/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences = s
  .flatMap(word => wordOccurrences(word))
  .groupBy{case (key, value) => key}
  .mapValues(_.reduce( (a, b) => (a._1, a._2 + b._2)))
  .mapValues{ case (a: Char, b: Int) => b}
  .toList
  .sortBy{ case(key, value) => key}


sentenceOccurrences(sentence1)



val dictionary: List[Word] = List("Ana", "are", "mere", "ate", "eat", "tea")

val listOfMap = dictionary
  .flatMap( word => Map(wordOccurrences(word) -> word))
  .groupBy{ case (occ: Occurrences, _: String) => occ}
  .mapValues{ c => c.map{ case (_, words) => words}}

val flattened = listOfMap
  .flatten
  .groupBy{ case (occ: Occurrences, _: String) => occ}
//    .mapValues( _.reduce( (a: List[String], b: List[String]) => a :: b) )


val flattened2 = flattened.mapValues{ c => c.map{ case (_, words) => words}}



//def mergeMap(m1: Map[List[(Char, Int)], List[String]],  m2: Map[List[(Char, Int)], List[String]]): Map[List[(Char, Int)], List[String]] = {
//  var map : Map[List[(Char, Int)], List[String]] = Map[List[(Char, Int)], List[String]]() ++ m1
//  for(p <- m2) {
//    map = map + (p._1 -> (p._2 :: map.getOrElse(p._1, List[String]())))
//  }
//  map
//}



//val folded = listOfMap.foldLeft(Map){ case (a, (k, v)) => k ++ v }

// Map[Occurrences, List[Word]] = dictionary.map( _ => Map(wordOccurrences(_) -> List(_)))
//

val list = (1 to 10).toList

// imutable map
val map1 = list.foldLeft(Map.empty[Int,String])( (map, value) => map + (value -> value.toString) )

map1.toList


