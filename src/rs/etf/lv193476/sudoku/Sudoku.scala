package rs.etf.lv193476.sudoku

class Sudoku(initial: Board, player: Player) {
  val initialWithCoords = initial.appendCoordinatesToFields
  val originalFields = initialWithCoords.map((row => row.filter(x => x._2 != '-')))
  val subCoords = for (x <- 0 to 8 by 3; y <- 0 to 8 by 3) yield (x, y)
  
  def isSolvable(board: Board): Boolean = {
    true
  }
  
  private def isLegal(lst: List[Char]): Boolean = {
    val sum = lst.map {
      case x if x.isDigit => x.asDigit
      case _ => 0
    }.distinct.sum
    sum == 45
  }
  
  def isOver(current: Board): Boolean = {
    val allRowsAreLegal = current.state forall isLegal
    val allColsAreLegal = current.transpose.state forall isLegal
    val submatrixes = subCoords.map(coord => current.subMatAsList(coord._1, coord._2)).toList
    val allSubsAreLegal = submatrixes forall isLegal
    allRowsAreLegal && allColsAreLegal && allSubsAreLegal
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
      next match {
        case x if isOver(x) => x
        case y if isLegit(y) => play(y)
        case _ => play(current)
      }
    }
    println("game start")
    println(s"${player.name} is playing")
    play(initial)
    println("You completed this sudoku!")
  }
  
}