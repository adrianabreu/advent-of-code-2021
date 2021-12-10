package submarine
import scala.io.Source

object Syntax {

  def main(args: Array[String]): Unit = {
    val filename            = "input10.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val errorScore = calculateErrors(lines)
    println(errorScore)

    val completeScore = calculateIncomplete(lines)
    println(completeScore)
  }

    val opposals = Map(
    ")" -> "(",
    "]" -> "[",
    "}" -> "{",
    ">" -> "<"
  )

  def calculateErrors(xs: List[String]): Int = {
    val errorsScore  = Map(
    ")" -> 3,
    "]" -> 57,
    "}" -> 1197,
    ">" -> 25137)

    xs.map(x => findCorruptedLines(x.split("").toList)).filter(a => !errorsScore.keys.find(_ == a.head).isEmpty).map(a => errorsScore(a.head)).sum 
  }

  def calculateIncomplete(xs: List[String]): Long = {
     val completeScore  = Map(
      "(" -> 1,
      "[" -> 2,
      "{" -> 3,
      "<" -> 4)

    def go(ys: List[String], acc: Long): Long = {
      ys match {
        case Nil => acc
        case y :: next => go(next, acc * 5 + completeScore(y))
      }
    }

    val scores = xs.map(x => findCorruptedLines(x.split("").toList)).filter(a => !completeScore.keys.find(_ == a.head).isEmpty).map(go(_, 0)) 


  

    scores.sortWith(_ < _)(scores.size / 2)
  }

   

  def findCorruptedLines(ys: List[String]): List[String] = {


    def loop(ys: List[String], xs: List[String]): List[String] = {
      ys match {
        case Nil => xs
        case head :: next => if (!opposals.keys.find(_ == head).isEmpty) {
          if (opposals(head) == xs.head) loop(next, xs.tail)
          else head +: xs
        } else loop(next,  head +: xs)
      }
    }
    loop(ys, List())
  }

}

