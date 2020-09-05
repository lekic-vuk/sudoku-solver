package rs.etf.lv193476.sudoku

object Hello extends App {
  override def main(args: Array[String]): Unit = {
    println("hello!")
    val board = """4-129--75
      |2--3--8-P
      |-7--8---6
      |---1-3-62
      |1-5---4-3
      |73-6-8---
      |6---2--3-
      |--7--1--4
      |89--651-7""".stripMargin
    
    val boardState = board.split('\n').map(s => s.map(c => c.toString).toList).toList
    val posX = boardState.indexWhere(row => row.contains("P"))
    val posY = boardState.find(row => row.contains("P")) match {
      case Some(x) => x.indexOf("P")
      case None => 0
    }
    val boardStateWithoutP = boardState.updated(posX, boardState(posX).updated(posY, "-"))
    val sudokuBoard = new Board(boardStateWithoutP, (posX, posY))
    val player = new Player("Vuk", sudokuBoard)
    val game = new Sudoku(sudokuBoard, player)
    game.start(player.turn)
  }
}