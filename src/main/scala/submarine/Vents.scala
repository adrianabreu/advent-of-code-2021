package submarine
import scala.io.Source

object Vents {

  def main(args: Array[String]): Unit = {
    val filename = "input5.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val overlaps = overlappings(lines)
    println(overlaps)

    val overlapsDiagonals = overlappingsWithDiagonals(lines)
    println(overlapsDiagonals)
  }

  def overlappings(xs: List[String]): Int = {
    val coords: List[Coordinate] = xs.flatMap(
      _.split(" -> ").toList.map(a =>
        Coordinate(a.split(",").head.toInt, a.split(",").last.toInt)
      )
    )

    val allPoints = coords
      .sliding(2, 2)
      .toList
      .map(_.sortWith((a, b) => a.x < b.x || a.y < b.y))
      .filter { case List(a, b) => a.x == b.x || a.y == b.y }
      .map { case List(a, b) => Coordinate.generateRange(a, b) }
      .reduce(_ ++ _)
    allPoints.groupBy(identity).mapValues(_.size).count(_._2 > 1)
  }

  def overlappingsWithDiagonals(xs: List[String]): Int = {
    val coords: List[Coordinate] = xs.flatMap(
      _.split(" -> ").toList.map(a =>
        Coordinate(a.split(",").head.toInt, a.split(",").last.toInt)
      )
    )

    val allPoints = coords
      .sliding(2, 2)
      .toList
      .map(_.sortWith((a, b) => a.x < b.x))
      .filter { case List(a, b) =>
        a.x == b.x || a.y == b.y || ((a.x - b.x).abs == (a.y - b.y).abs)
      }
      .map { case List(a, b) => Coordinate.generateRangeForDiagonals(a, b) }
      .reduce(_ ++ _)
    val map = allPoints.groupBy(identity).mapValues(_.size)

    allPoints.groupBy(identity).mapValues(_.size).count(_._2 > 1)
  }

}

case class Coordinate(x: Int, y: Int)

object Coordinate {
  def generateRange(a: Coordinate, b: Coordinate): List[Coordinate] = {
    val constant = if (a.x == b.x) a.x else a.y
    val list =
      (a.x).to(b.x).zipAll((a.y).to(b.y), constant, constant).toList.map {
        case (x, y) => Coordinate(x, y)
      }
    list
  }

  def generateRangeForDiagonals(
      a: Coordinate,
      b: Coordinate
  ): List[Coordinate] = {
    val constant = if (a.x == b.x) a.x else a.y
    val step = if (b.y < a.y) -1 else 1
    val list =
      (a.x).to(b.x).zipAll((a.y).to(b.y, step), constant, constant).toList.map {
        case (x, y) => Coordinate(x, y)
      }
    list
  }
}
