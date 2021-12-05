package submarine
import scala.io.Source

object Position {

  def main(args: Array[String]): Unit = {
    val filename = "input2.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val position = calculate(lines)
    println(position)

    println(position.x * position.y)

    val positionWithAim = calculateWithAim(lines)
    println(positionWithAim)
    println(positionWithAim.x * positionWithAim.y)
  }

  def calculate(xs: List[String]): Position =
    xs.foldRight(new Position(0, 0)) { case (value, acc) =>
      value match {
        case f if value.startsWith("forward") =>
          new Position(acc.x + f.split(" ").last.toInt, acc.y)
        case d if value.startsWith("down") =>
          new Position(acc.x, acc.y + d.split(" ").last.toInt)
        case u if value.startsWith("up") =>
          new Position(acc.x, acc.y - u.split(" ").last.toInt)
      }
    }

  def calculateWithAim(xs: List[String]): Position =
    xs.foldLeft(new Position(0, 0)) { case (acc, value) =>
      value match {
        case f if value.startsWith("forward") =>
          new Position(
            acc.x + f.split(" ").last.toInt,
            acc.y + (acc.aim * f.split(" ").last.toInt),
            acc.aim
          )
        case d if value.startsWith("down") =>
          new Position(acc.x, acc.y, acc.aim + d.split(" ").last.toInt)
        case u if value.startsWith("up") =>
          new Position(acc.x, acc.y, acc.aim - u.split(" ").last.toInt)
      }
    }

}

case class Position(x: Int, y: Int, aim: Int = 0)
