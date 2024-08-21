package org.scala.minesweeper

import org.scala.minesweeper

import util.control.Breaks.*
import scala.io.StdIn

sealed trait Cell
case class Mijn(open: Boolean) extends Cell
case class Leeg(open: Boolean) extends Cell
case class Nummer(num: Int) extends Cell
case class Marked(cell: Cell) extends Cell

case class GridFunc(cells: List[List[Cell]])

def markFunc(grid: GridFunc, x: Int, y: Int): GridFunc = {
  grid.cells(y)(x) match {
    case Marked(cell) =>
      updateGrid(grid, x, y, cell)
    case cell: Cell =>
      updateGrid(grid, x, y, Marked(cell))
  }
}

private def totalOpenedFunc(grid: GridFunc): Int = {
  grid.cells.map(
    arr => arr.count {
      case Mijn(o) if o => true
      case Nummer(n) => true
      case Leeg(o) if o => true
      case Marked(_) => true
      case _ => false
    }
  ).sum
}

private def nonMineCellsFunc(grid: GridFunc) = {
  grid.cells.map(
    arr => arr.count {
      case Mijn(_) => false
      case _ => true
    }
  ).sum
}

private def isGameWonFunc(grid: GridFunc): Boolean = {
  val totalOpen = totalOpenedFunc(grid)
  totalOpen == nonMineCellsFunc(grid)
}

private def isGameLostFunc(grid: GridFunc): Boolean = {
  grid.cells.exists(row => row.contains(Mijn(true)))
}


def openFunc(grid: GridFunc, x: Int, y: Int): GridFunc = {
  if (NotInboundsFunc(grid, x, y))
    return grid

  val cellSurroundingMines: List[(Int, Int)] = numSurroundingMinesFunc(grid, x, y)
  if (cellSurroundingMines.nonEmpty) {
    updateGrid(grid, x, y, Nummer(cellSurroundingMines.size))
  } else {
    grid.cells(y)(x) match {
      case Mijn(o) if !o =>
        updateGrid(grid, x, y, Mijn(true))
      case Leeg(o) if !o =>
        val newGrid = updateGrid(grid, x, y, Leeg(true))
        dfsFunc(newGrid, x, y)
      case _ => grid
    }
  }
}

def numSurroundingMinesFunc(grid: GridFunc, x: Int, y: Int): List[(Int, Int)] = {
  val surroundingCoords = createCoordsSurroundingCellsFunc(
    grid,
    (x - 1 to x + 1).toList,
    (y - 1 to y + 1).toList,
    (x, y)
  )
  surroundingCoords.filter(
    (xb, yb) =>
      !NotInboundsFunc(grid, xb, yb) && (xb, yb) != (x, y) &&
        ( grid.cells(yb)(xb) == Mijn(false) || grid.cells(yb)(xb) == Mijn(true) )
  )
}

private def NotInboundsFunc(grid: GridFunc, x: Int, y: Int): Boolean =
  (y < 0 || y >= grid.cells.length) ||
    (x < 0 || x >= grid.cells.head.length)

private def createCoordsSurroundingCellsFunc(
                                          grid: GridFunc,
                                          xr: List[Int],
                                          yr: List[Int],
                                          currCood: (Int, Int)
                                        ): List[(Int, Int)] = {

  val mapped: List[(Int, Int)] = xr.flatMap(x => yr.map(y => (x, y)))
  mapped.filter((x, y) => !NotInboundsFunc(grid, x, y) && (x, y) != currCood)

}

private def dfsFunc(grid: GridFunc, x: Int, y: Int): GridFunc = {

  println(s"curr x,y: $x, $y")
  val l_coords: List[(Int, Int)] = createCoordsSurroundingCellsFunc(
    grid,
    (x - 1 to x + 1).toList,
    (y - 1 to y + 1).toList,
    (x, y)
  )

  // Fold over the coordinates to accumulate the updated grid
  l_coords.foldLeft(grid) { (accGrid, coord) =>
    val (xb, yb) = coord
    val currCell = accGrid.cells(yb)(xb)
    val surroundingMines = numSurroundingMinesFunc(accGrid, xb, yb)
    if (surroundingMines.nonEmpty) {
      updateGrid(accGrid, xb, yb, Nummer(surroundingMines.size))
    } else {
      currCell match {
        case Leeg(o) if !o =>
          val newGrid = updateGrid(accGrid, xb, yb, Leeg(true))
          dfsFunc(newGrid, xb, yb) // Recursively call with the updated grid
        case _ => accGrid // Return the accumulator grid unchanged
      }
    }
  }
}

def updateGrid(grid: GridFunc, i_x: Int, i_y: Int, new_val: Cell): GridFunc = {
  val new_lst: List[Cell] = grid.cells(i_y).updated(i_x, new_val)
  GridFunc(grid.cells.take(i_y) ::: new_lst :: grid.cells.drop(i_y + 1))
}

def gridFuncPrint(grid: GridFunc, gameEnded: Boolean = false): Unit = {
  for (w <- grid.cells.head.indices)
    print("---")
  println()

  for (y <- grid.cells.indices) {
    print("| ")

    for (x <- grid.cells.head.indices) {
      val currCell = grid.cells(y)(x)
      currCell match {
        case Mijn(open) =>
          if (open) print("MO  | ") else
            if (gameEnded) print("  M   | ") else print("     | ")
        case Leeg(open) => if (open) print("  O   | ") else print("     | ")
        case Nummer(n) => print(s"N($n) | ")
        case Marked(_) => print("F | ")
      }
    }

    println()
  }

  for (w <- grid.cells.head.indices)
    print("---")

  println()
}

private val VALID_ACTIONS_STR: List[String] = List(
  "o",
  "m"
)

@main
def main(): Unit = {

  // TODO: dynamically initialize the first grid using width and height values
  val init_cells: List[List[Cell]] = List(
    List(Mijn(false), Leeg(false), Leeg(false), Leeg(false), Leeg(false)),
    List(Leeg(false), Leeg(false), Leeg(false), Leeg(false), Leeg(false)),
    List(Leeg(false), Leeg(false), Mijn(false), Mijn(false), Leeg(false)),
    List(Leeg(false), Leeg(false), Leeg(false), Leeg(false), Leeg(false)),
    List(Leeg(false), Leeg(false), Leeg(false), Leeg(false), Leeg(false))
  )

//  val height: Int = 5
//  val width: Int = 5

  var grid = GridFunc(init_cells)

  var won: Boolean = false
  var lost: Boolean = false

  while (!won && !lost) {
    breakable {
      gridFuncPrint(grid)

      val splitInput = StdIn.readLine(
        "Input coordinates in the form action{o,m},x,y including the comma: "
      ).split(",")

      if (splitInput.size != 3) {

        println(s"Provide args of the following format: action,x,y. Actions possible: $VALID_ACTIONS_STR")
        break

      }

      val action: String = splitInput(0).strip
      val splitInputX: Int = splitInput(1).strip.toInt
      val splitInputY: Int = splitInput(2).strip.toInt

      println(s"x = $splitInputX")
      println(s"y = $splitInputY")

      action match
        case "o" =>
          grid = openFunc(grid, splitInputX, splitInputY)
          if (isGameWonFunc(grid)) won = true
          else if (isGameLostFunc(grid)) lost = true
        case "m" => 
          grid = markFunc(grid, splitInputX, splitInputY)
        case _ => println(s"Invalid action provided: $action. Valid actions: $VALID_ACTIONS_STR")

      println(s"Opened Total: ${totalOpenedFunc(grid)}")
      println(s"Amount To Open To Win: ${nonMineCellsFunc(grid)}")
    }
  }

  if (lost) {
    println("You have lost!")
  } else {
    println("You have won!")
  }

  gridFuncPrint(grid, gameEnded = true)
}