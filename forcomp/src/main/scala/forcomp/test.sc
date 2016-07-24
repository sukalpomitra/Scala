import forcomp._

val dictionaryPath = List("forcomp", "linuxwords.txt")

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word): Occurrences = {
  val wPairs = (w filter(e => e.isLetter)).toLowerCase.groupBy(identity).toList
  wPairs.sorted map (x => (x._1,x._2.length))

}

def sentenceOccurrences(s: Sentence): Occurrences = {
  wordOccurrences(s mkString(""))
}

val dictionary = List("java", "is")

lazy val dictionaryByOccurrences: Unit = {
  dictionary groupBy(word => wordOccurrences(word))

}

val lst  = List(('a', 2), ('b', 2))

def combinations(occurrences: Occurrences): List[Occurrences] = {
  val ocs : List[Occurrences] = occurrences.map( x => (for(i <- 1 until (x._2+1)) yield (x._1,i)).toList)
  ocs.foldRight(List[Occurrences](Nil))((x,y) => y ++ (for(i <- x; j <- y) yield (i :: j)))
}

val res = wordOccurrences("ok")
println(res)

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val result = (for {a <- x
       i <- y
       if ((a._1 != i._1) || (a._1 == i._1 && i._2 > a._2)) }
    yield a)
  if (result.isEmpty) List() else result.sorted
}

//val lst1 =
val res2 = wordOccurrences("ok")
//println(res1)
val sub = subtract(res, res2)

println("hello")