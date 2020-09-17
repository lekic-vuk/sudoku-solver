package main.scala

object Code extends Enumeration {
  type Code = Value
  val Save, Move = Value
}
import Code._

case class BoardEditor(board: Board) {
  type Command = BoardEditor => (Code, BoardEditor)
  type Operation = Board => Board
  
  def transpose: BoardEditor =
    BoardEditor(board.transpose)
  def replace: BoardEditor =
    BoardEditor(Board(board.state.map(v => v.map(c => c match {
      case x if x.asDigit >= 1 && x.asDigit <= 9 => (10 - c.asDigit).toString.head
      case _ => c
    })), board.pencil))
  
  def id: Operation = b => b
  
  def compose(f: Operation , g: Operation ): Operation  =
    b => f(g(b))
  
  def makeSequenceOfOps(ops: List[Operation]): Operation =
    ops.foldLeft(id)((l, r) => compose(r, l))
    
  def movePencil(p: Pencil): Operation =
    board => Board(board.state, p)
    
  def write(c: Char): Operation =
    board => board.write(c)
    
  def remove: Operation =
    board => board.write('-')
    
  def setStartPosition(x: Int, y: Int): Operation =
      writeAt(x, y, '-')
  
  def writeAt(x: Int, y: Int, c: Char) =
    compose(write(c), movePencil(Pencil(x, y)))
    
  def isSolvable(board: Board): Boolean = {
    true
  }
    
  def start() {
    def edit(editor: BoardEditor): BoardEditor = {
      editor.board.printBoard
      val command = scala.io.StdIn.readLine
      command match {
        case x if x == "d" || x == "u" || x == "l" || x == "r" =>
          edit(BoardEditor(editor.board.movePencil(x.head)))
        case x if "123456789".exists(_ == x.head) =>
          edit(BoardEditor(editor.board.write(x.head)))
        case x if x == "transpose" =>
          edit(editor.transpose)
        case x if x == "replace" => {
          val newEditor = edit(editor.replace)
          newEditor
        }
        case _ => edit(this)
      }
    }
    edit(this)
  }
    
}