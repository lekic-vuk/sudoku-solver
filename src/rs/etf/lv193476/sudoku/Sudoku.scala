package rs.etf.lv193476.sudoku

class Sudoku(initial: Board, player: Player) {
  val initialWithCoords = appendCoordinatesToFields(initial)
  val originalFields = initialWithCoords.map((row => row.filter(x => x._2 != "-")))
  
  def isSolvable(board: Board): Boolean = {
    true
  }
  
  private def appendCoordinatesToFields(board: Board) = {
    val withColumnIndex = board.state.map(row => (0 to row.length - 1).zip(row))
    val withRowIndex = (0 to board.state.length - 1).zip(withColumnIndex)
    withRowIndex.map(row => row._2.map(elem => ((row._1, elem._1), elem._2)))
  }
  
  def isOver(current: Board): Boolean = {
    false
  }
  
  def isLegit(board: Board): Boolean = {
    originalFields.forall(row => row.forall(element => {
      val x = element._1._1
      val y = element._1._2
      val ch = element._2
      board.state(x)(y) == ch
    }))
  }

  def start(turn: Board => Board) {
    def play(current: Board): Board = {
      val next = turn(current)
      if (isOver(next)) next
      else play(if (isLegit(next)) next else current)
    }
    println("game start")
    println(s"${player.name} is playing")
    play(initial)
  }
  
}