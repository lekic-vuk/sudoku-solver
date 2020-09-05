package rs.etf.lv193476.sudoku

import scala.io.StdIn
import Console.{RESET, RED, BLUE, BLACK, REVERSED}

class Player(val name: String, initialBoard: Board) {
  private def printWithColors(currentBoard: Board) = {
      println(s"${BLACK}current state")
      currentBoard.state.indices.foreach(row => {
        currentBoard.state(row).indices.foreach(col => {
          val backgroundColor = (row, col) match {
            case (currentBoard.pencilPos._1, currentBoard.pencilPos._2) => REVERSED
            case _ => RESET
          }
          val textColor = initialBoard.state(row)(col) match {
            case "-" => BLUE
            case _ => RED
          }
          print(backgroundColor + textColor + currentBoard.state(row)(col))
          }
        )
        println
      })
    }
  val turn = (currentBoard: Board) => {
    print("\u001b[2J")
    printWithColors(currentBoard)
    println(s"${RESET}playing...")
    val c = scala.io.StdIn.readChar()
    val nextBoard = c match {
      case 'd' => currentBoard.movePencilDown
      case 'u' => currentBoard.movePencilUp
      case 'l' => currentBoard.movePencilLeft
      case 'r' => currentBoard.movePencilRight
      case d if d.isDigit => {
        val x = currentBoard.pencilPos._1
        val y = currentBoard.pencilPos._2
        val nextState = currentBoard.state.updated(x, currentBoard.state(x).updated(y, d.toString))
        new Board(nextState, currentBoard.pencilPos)
      }
      case _ => currentBoard
    }
    nextBoard
  }
}