package main.scala
import Console.{RESET, RED, BLUE, BLACK, REVERSED}


case class Board(val state: Vector[Vector[Char]], val pencil: Pencil) {
  val initial = state
  type Row = List[Char]
  type Matrix = List[Row]
  
  def appendCoordinatesToFields = {
    val withColumnIndex = state.map(row => (0 to row.length - 1).zip(row))
    val withRowIndex = (0 to state.length - 1).zip(withColumnIndex)
    val ret = withRowIndex.map(row => row._2.map(elem => ((row._1, elem._1), elem._2)).toList).toList
    ret
  }
  
  def transpose(): Board = {
    def _transposeMatrix(mat: Vector[Vector[Char]]): Vector[Vector[Char]] = {
      mat.filter(_.nonEmpty) match {
        case Vector() => Vector()
        case _ =>  mat.map(_.head) +: _transposeMatrix(mat.map(_.tail))
      }
    }
    Board(_transposeMatrix(state), pencil)
  }
  
  // sub-matrix is defined as 3x3 matrix whose top left coordinates
  // take values from {0, 3, 6} set and have to contain all numbers from [1,9] range
  def subMatAsVector(x: Int, y: Int): Vector[Char] = {
    val stateWithCoords = this.appendCoordinatesToFields
    stateWithCoords.flatMap(row =>
      row.filter(el =>
        el._1._1 >= x && el._1._1 < x + 3 && el._1._2 >= y && el._1._2 < y + 3)
         .map(_._2)).toVector
  }
  
  def write(value: Char) = {
    val x = pencil.posX
    val y = pencil.posY
    val nextState = state.updated(x, state(x).updated(y, value))
    Board(nextState, pencil)
  }
  
  def changePencil(newPencil: Pencil): Board =
    Board(state, newPencil)
  
  def movePencil(towards: Char): Board =
    Board(state, pencil.move(towards))
    
  def printBoard: Unit = {
    state.indices.foreach(row => {
      state(row).indices.foreach(col => {
        val backgroundColor = (row, col) match {
          case (pencil.posX, pencil.posY) => REVERSED
          case _ => RESET
        }
        print(backgroundColor + state(row)(col))
      })
      println
    })
  }
  
  def printWithColors(initialBoard: Board): Unit = {
    print("\u001b[2J")
    println(s"${BLACK}current state")
    Console.flush()
    state.indices.foreach(row => {
      state(row).indices.foreach(col => {
        val backgroundColor = (row, col) match {
          case (pencil.posX, pencil.posY) => REVERSED
          case _ => RESET
        }
        val textColor = initialBoard.state(row)(col) match {
          case '-' => BLUE
          case _ => RED
        }
        print(backgroundColor + textColor + state(row)(col))
        }
      )
      println
    })
  }
}

object Board {
  def empty(): Board = {
    val emptyState = """---------
      |---------
      |---------
      |---------
      |---------
      |---------
      |---------
      |---------
      |---------""".stripMargin
                   .split('\n').map(s => s.toVector).toVector
    Board(emptyState, Pencil(0, 0))
  }
}