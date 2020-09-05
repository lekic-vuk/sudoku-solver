package rs.etf.lv193476.sudoku

class Board(val state: List[List[String]], val pencilPos: (Int, Int)) {
  val initial = state
  type Row = List[Char]
  type Matrix = List[Row]
  
  private def movePencil(move: ((Int, Int)) => (Int, Int)): Board =
    new Board(state, move(pencilPos))
  
  def movePencilLeft(): Board = {
    val left = (prevPos: (Int, Int)) => {
      val y = prevPos._2
      val newPosY = java.lang.Math.floorMod(y - 1, 9)
      (pencilPos._1, newPosY)
    }
    movePencil(left)
  }
  def movePencilRight(): Board = {
    val right = (prevPos: (Int, Int)) => {
      val y = pencilPos._2
      val newPosY = (y + 1) % 9
      (pencilPos._1, newPosY)
    }
    movePencil(right)
  }
  def movePencilDown(): Board = {
    val down = (prevPos: (Int, Int)) => {
      val x = pencilPos._1
      val newPosX = (x + 1) % 9
      (newPosX, pencilPos._2)
    }
    movePencil(down)
  }
  def movePencilUp(): Board = {
    val up = (prevPos: (Int, Int)) => {
      val x = pencilPos._1
      val newPosX = java.lang.Math.floorMod(x - 1, 9)
      (newPosX, pencilPos._2)
    }
    movePencil(up)
  }
}