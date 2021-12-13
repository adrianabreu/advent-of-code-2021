package submarine
import scala.io.Source

object Origami {

  def main(args: Array[String]): Unit = {
    val filename            = "input13.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val part1 = fold(lines, Some(1)).size
    println(part1)

    val part2 = fold(lines)
    visualize(part2)
  }

  def fold(xs: List[String], steps: Option[Int] = None): Set[(Int, Int)] = {
    val points = xs
      .takeWhile(p => p != "")
      .map(p => {
        val t = p.split(",").map(_.toInt)
        (t(0), t(1))
      })
      .toSet
    val fold = xs
      .dropWhile(p => p != "")
      .drop(1)
      .map(f => {
        val c = f.drop(11).split("=")
        (c(0), c(1))
      })

    def loop(fold: List[(String, String)], points: Set[(Int, Int)]): Set[(Int, Int)] = {
      fold match {
        case Nil => points
        case a :: next =>
          a match {
            case ("x", n) =>
              loop(
                next,
                points.filter(p => p._1 < n.toInt) ++ points
                  .filter(p => p._1 > n.toInt)
                  .map(p => {
                    val d = n.toInt
                    val x = p._1 - d
                    (d - x, p._2)
                  })
              )
            case ("y", n) =>
              loop(
                next,
                points.filter(p => p._2 < n.toInt) ++ points
                  .filter(p => p._2 > n.toInt)
                  .map(p => {
                    val d = n.toInt
                    val x = p._2 - d
                    (p._1, d - x)
                  })
              )
          }
      }

    }

    val actualFold = steps match {
      case None    => fold
      case Some(x) => fold.take(x)
    }

    loop(actualFold, points)

  }

  def visualize(z: Set[(Int, Int)]): Unit = {
    val maxX = z.map(f => f._1).max
    val maxY = z.map(f => f._2).max

    val grid = 0
      .to(maxY)
      .map(r => {
        0.to(maxX).map(c => if (z.find(d => d == (c, r)).isEmpty) "·" else "#").mkString
      })

    for (g <- grid) { println(g) }
  }
}
