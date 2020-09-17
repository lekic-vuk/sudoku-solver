package main.scala

import Console.{RESET, RED, BLUE, BLACK, REVERSED}

class Player(val name: String, initialBoard: Board) {
  private def printWithColors(currentBoard: Board) = {
    println(s"${BLACK}current state")
    currentBoard.state.indices.foreach(row => {
      currentBoard.state(row).indices.foreach(col => {
        val backgroundColor = (row, col) match {
          case (currentBoard.pencil.posX, currentBoard.pencil.posY) => REVERSED
          case _ => RESET
        }
        val textColor = initialBoard.state(row)(col) match {
          case '-' => BLUE
          case _ => RED
        }
        print(textColor + backgroundColor + currentBoard.state(row)(col))
        }
      )
      println
    })
  }
  
  def turn (think: () => Char)(currentBoard: Board) = {
      print("\u001b[2J")
      currentBoard.printWithColors(initialBoard)
      println(s"${RESET}playing...")
      think() match {
        case value if value.isDigit => currentBoard.write(value)
        case direction => currentBoard.movePencil(direction)
      }
    }
  
}