package submarine
import scala.io.Source

object Segments {

  def main(args: Array[String]): Unit = {
    val filename            = "input8.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val digits = easyDigits(lines)
    println(digits)

    val decodedDigits = decodeDigits(lines)
    println(decodedDigits)   
  }

  def easyDigits(xs: List[String]): Int = {
    val input = xs.flatMap(_.split('|').last.split(" ").map(_.size)).groupBy(identity).mapValues(_.size.toInt)

    input.filter(x => List(2,3,4,7).contains(x._1)).values.sum
  }

  def decodeDigits(xs: List[String]): Int = {
    val input = xs.map(_.split('|')) 
    
    input.map( a => {
      val signals = mapSignals(a.head)      
      translate(a.last, signals)
    }).sum
  }

  def mapSignals(signals: String): Map[String, String] = {
    val lengthMap = signals.split(" ").groupBy(_.size)

    val setCF = lengthMap(2).head.toSet
    val setACF = lengthMap(3).head.toSet
    val setBCDF = lengthMap(4).head.toSet
    val setA = setACF -- setCF
    val setG = lengthMap(6).map(_.toSet -- setA -- setBCDF).find(_.size == 1).get
    val setD = lengthMap(5).map(_.toSet -- setG -- setACF).find(_.size == 1).get
    val setB = setBCDF -- setCF -- setD
    val setE = lengthMap(5).map(_.toSet -- setBCDF -- setA -- setG).find(_.size == 1).get
    val setF = lengthMap(6).map(_.toSet -- setA -- setB -- setG -- setD --setE).find(_.size == 1).get
    val setC = setCF -- setF

    Map(
       setA.head.toString -> "a",
       setG.head.toString -> "g",
       setD.head.toString -> "d",
       setB.head.toString -> "b",
       setE.head.toString -> "e",
       setF.head.toString -> "f",
       setC.head.toString -> "c"
    )
  }

  def translate(digits: String, mapping: Map[String, String]): Int = {
      digits.split(" ").filter(!_.isEmpty).map(d => {
        val mapped = d.split("").map(mapping(_)).sorted.mkString
        Pattern.knownPatterns(mapped)
      }).mkString.toInt
  } 

}

object Pattern {

  val knownPatterns = Map(
    "abcefg" -> "0",
    "cf" -> "1",
    "acdeg" -> "2",
    "acdfg" -> "3",
    "bcdf" -> "4",
    "abdfg" -> "5",
    "abdefg" -> "6",
    "acf" -> "7",
    "abcdefg" -> "8",
    "abcdfg" -> "9"
  )
}