package submarine
import scala.io.Source


object Bingo {

  def main(args: Array[String]): Unit = {
    val filename = "input4.txt"
    val lines: List[String] = Source.fromResource(filename).getLines.toList

    val winnerPoints = play(lines)
    println(winnerPoints)

    val lastWinPoints = playCheating(lines)
    println(lastWinPoints)

  }


  def play(xs:List[String]): Int = {
    val numbers = xs.head.split(",").map(_.toInt).toList
    val boardMarks = xs.tail.filter(a => !a.isEmpty).map(l => l.split(" ").filter(a => !a.isEmpty).map(a => BoardMark(a.toInt, false)).toList)

    val boards = boardMarks.sliding(5, 5).map(l => Board(l)).toList

    def playUntilWinner(numbers: List[Int], boards: List[Board], possibleWinner: Int): (Board, Int) = {
        val winner = boards.filter(p => p.rows.exists(bm  => bm.forall(_.checked) || p.rows.transpose.exists(bm  => bm.forall(_.checked))))
        if (!winner.isEmpty) (winner.head, possibleWinner)
        else {
          val n = numbers.head
          val updatedBoards = boards.map(
          b => Board(b.rows.map(r => r.map(bm => if (bm.number == n) BoardMark(bm.number, true) else bm)))
          )
          playUntilWinner(numbers.tail, updatedBoards, n)
        }
    }
    val winner = playUntilWinner(numbers, boards, 0)
    calculatePoints(winner._1.rows, winner._2)

  }

  def playCheating(xs:List[String]): Int = {
    val numbers = xs.head.split(",").map(_.toInt).toList
    val boardMarks = xs.tail.filter(a => !a.isEmpty).map(l => l.split(" ").filter(a => !a.isEmpty).map(a => BoardMark(a.toInt, false)).toList)

    val boards = boardMarks.sliding(5, 5).map(l => Board(l)).toList

    def playUntilLastWin(numbers: List[Int], boards: List[Board], possibleWinner: Int): (Board, Int) = {
        val winner = boards.filter(p => p.rows.exists(bm  => bm.forall(_.checked) || p.rows.transpose.exists(bm  => bm.forall(_.checked))))
        if (!winner.isEmpty && boards.size == 1) (winner.head, possibleWinner)
        else {
          // Would be good to just subtract from winners
          val nonWinningBoards = boards.filter(p => !p.rows.exists(bm  => bm.forall(_.checked) || p.rows.transpose.exists(bm  => bm.forall(_.checked))))
          val n = numbers.head
          val updatedBoards = nonWinningBoards.map(
          b => Board(b.rows.map(r => r.map(bm => if (bm.number == n) BoardMark(bm.number, true) else bm)))
          )
          playUntilLastWin(numbers.tail, updatedBoards, n)
        }
    }
    val winner = playUntilLastWin(numbers, boards, 0)
    calculatePoints(winner._1.rows, winner._2)

  }

  def calculatePoints(xs: List[List[BoardMark]], number: Int): Int =
    xs.map(x => x.filter(!_.checked).map(_.number).sum).sum * number
}

case class BoardMark(number: Int, checked: Boolean)
case class Board(rows: List[List[BoardMark]])