import objsets._

val set1 = new Empty
val set2 = set1.incl(new Tweet("a", "a body", 20)) // {a}
val set3 = set2.incl(new Tweet("b", "b body", 20)) // {a, b}
val c = new Tweet("c", "c body", 7) //
val d = new Tweet("d", "d body", 9) //
val set4c = set3.incl(c) // {a, b, c}
val set4d = set3.incl(d) // {a, b, d}
val set5 = set4c.incl(d) // {a, b, c, d}

val f5 = set5.filter(tw => tw.retweets == 20)
