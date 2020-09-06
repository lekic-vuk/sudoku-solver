package rs.etf.lv193476.sudoku

class Pencil(val posX: Int, val posY: Int) {
  def move(direction: Char) =
    direction match {
      case 'd' => moveDown()
      case 'u' => moveUp()
      case 'l' => moveLeft()
      case 'r' => moveRight()
      case _ => this
  }
  
  private def movePencil(move: (Pencil) => Pencil) =
    move(this)
    
  private def left(pencil: Pencil) = {
    val newY = java.lang.Math.floorMod(pencil.posY - 1, 9)
    new Pencil(pencil.posX, newY)
  }
  private def right (pencil: Pencil) = {
      val newY = (pencil.posY + 1) % 9
      new Pencil(pencil.posX, newY)
    }
  private def down = (pencil: Pencil) => {
      val newX = (pencil.posX + 1) % 9
      new Pencil(newX, pencil.posY)
    }
  private def up(pencil: Pencil) = {
    val newX = java.lang.Math.floorMod(pencil.posX - 1, 9)
    new Pencil(newX, pencil.posY)
  }
  
  def moveLeft(): Pencil = { 
    movePencil(left)
  }
  
  def moveRight(): Pencil = {
    movePencil(right)
  }
  def moveDown(): Pencil =
    movePencil(down)
    
  def moveUp(): Pencil = {
    movePencil(up)
  }
}