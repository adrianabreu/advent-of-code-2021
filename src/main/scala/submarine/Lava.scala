package submarine
import scala.io.Source

object Lava {

  def main(args: Array[String]): Unit = {
    val filename            = "input9.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val risk = calculateRisk(lines)
    println(risk)

    val basins = calculateBasins(lines)
    println(basins)
  }

  def calculateRisk(xs: List[String]): Int = 
    findMinimums(xs).map(_.value + 1).reduce(_ + _)

  def findMinimums(xs: List[String]): List[Coord] = {
    val neighBourgs = List(
      (1, 0),
      (-1, 0),
      (0, 1),
      (0, -1)
    )
    val input = xs.map(_.split("").map(_.toInt).toList) 
    input.zipWithIndex.flatMap(t => {
      t._1.zipWithIndex.filter(u => {
        neighBourgs.map {
          case (a, b) => if ((a + t._2) >= 0 && (a + t._2) < input.size && (b + u._2) >= 0 && (b + u._2) < input(a + t._2).size) input(a + t._2)(b + u._2)
          else 99 
        }.forall(_ > u._1)
      }).map(u => Coord(u._1, t._2, u._2))
    })
  }

  def calculateBasins(xs: List[String]): Int = {
    val minimums = findMinimums(xs: List[String])

    val coords = xs.map(_.split("").map(_.toInt).toList).zipWithIndex.map(row => row._1.zipWithIndex)



    def loop(points: Set[(Int, Int)], visited:Set[(Int,Int)], n: Int): Int = {
      val isValid = (a: Int, b: Int) => (a >= 0 && b >= 0 && a < coords.size && b < coords(a).size && visited.find(_ == (a,b)).isEmpty && (coords(a)(b)._1 < 9))
      
      val neighbours = points.flatMap{ case(x, y) => List(
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1)
      )}
      
      val validNeighbours = neighbours.filter(x => isValid(x._1, x._2))

      if(validNeighbours.size == 0) n
      else loop(validNeighbours.toSet, visited ++ neighbours, n + validNeighbours.toSet.size)


    }
    minimums.map( m => {
      loop(Set((m.h, m.v)), Set((m.h, m.v)), 1)
    }).sortWith(_ > _).take(3).reduce(_ * _)
  }

}

case class Coord(val value: Int, val h: Int, val v: Int)
