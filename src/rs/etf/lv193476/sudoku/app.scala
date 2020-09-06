package rs.etf.lv193476.sudoku

object Hello extends App {
  override def main(args: Array[String]): Unit = {
    println("hello!")
    val boardAlmostFull = """P35269781
      |682571493
      |197834562
      |826195347
      |374682915
      |951743628
      |519326874
      |248957136
      |763418259""".stripMargin
      
    val board = """-32945861
      |14936872P
      |8652174-9
      |913624587
      |627851394
      |458793216
      |594182673
      |381576942
      |276439158""".stripMargin
      
    val board2 = """4-129--75
      |2--3--8-P
      |-7--8---6
      |---1-3-62
      |1-5---4-3
      |73-6-8---
      |6---2--3-
      |--7--1--4
      |89--651-7""".stripMargin
    
    val boardState = board.split('\n').map(s => s.toList).toList
    val posX = boardState.indexWhere(row => row.contains('P'))
    val posY = boardState.find(row => row.contains('P')) match {
      case Some(x) => x.indexOf('P')
      case None => 0
    }
    println(posX + " " + posY)
    val boardStateWithoutP = boardState.updated(posX, boardState(posX).updated(posY, '-'))
    val pencil = new Pencil(posX, posY)
    val sudokuBoard = new Board(boardStateWithoutP, pencil)
    val player = new Player("Vuk", sudokuBoard)
    val game = new Sudoku(sudokuBoard, player)
    game.start(player.turn)
  }
}