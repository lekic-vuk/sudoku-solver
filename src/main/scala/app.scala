package main.scala

object Menu extends App {
  override def main(args: Array[String]): Unit = {
    
    def mainLoop(quit: Boolean): Boolean = {
      println("commands: e = edit board; p = play game; q = quit program")
      if (quit) true
      val command = scala.io.StdIn.readChar
      command match {
        case 'e' => editBoard(); mainLoop(false)
        case 'p' => playGame(); mainLoop(false)
        case 'q' => mainLoop(true)
        case _ => mainLoop(false)
      }
    }
    mainLoop(false)
  }
  
  def editBoard(): Unit = {
    val board2 = """4-129--75
      |2--3--8-P
      |-7--8---6
      |---1-3-62
      |1-5---4-3
      |73-6-8---
      |6---2--3-
      |--7--1--4
      |89--651-7""".stripMargin
    val boardState = board2.split('\n').map(s => s.toVector).toVector
    val posX = boardState.indexWhere(row => row.contains('P'))
    val posY = boardState.find(row => row.contains('P')) match {
      case Some(x) => x.indexOf('P')
      case None => 0
    }
    println(s"pencilPosition: x = ${posX}, y = ${posY}")
    val boardStateWithoutP = boardState.updated(posX, boardState(posX).updated(posY, '-'))
    val editor = BoardEditor(Board.empty)
    
    editor.start
  }
  
  def playGame(): Unit = {
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
    
    val boardState = board.split('\n').map(s => s.toVector).toVector
    val posX = boardState.indexWhere(row => row.contains('P'))
    val posY = boardState.find(row => row.contains('P')) match {
      case Some(x) => x.indexOf('P')
      case None => 0
    }
    println(posX + " " + posY)
    val boardStateWithoutP = boardState.updated(posX, boardState(posX).updated(posY, '-'))
    val pencil = new Pencil(posX, posY)
    val sudokuBoard = new Board(boardStateWithoutP, pencil)
    val consolePlayer = new Player("Vuk", sudokuBoard)
    val game = new Sudoku(sudokuBoard, consolePlayer)
    game.start(consolePlayer.turn(scala.io.StdIn.readChar) _)
    println("end")
  }
}