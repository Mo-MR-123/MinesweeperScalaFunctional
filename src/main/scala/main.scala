package org.scala.minesweeper

import org.scala.minesweeper

import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn

// Immutable GameStatus Classes
sealed trait GameStatus
case object Won extends GameStatus
case object Lost extends GameStatus
case class InvalidInput(msg: String) extends GameStatus

// Immutable Cell Classes
sealed trait Cell
case class Mijn(open: Boolean) extends Cell
case class Leeg(open: Boolean) extends Cell
case class Nummer(num: Int) extends Cell
case class Marked(cell: Cell) extends Cell

// Immutable Grid 
case class Grid(cells: List[List[Cell]])

private def mark(grid: Grid, x: Int, y: Int): Grid = {
  grid.cells(y)(x) match {
    case Marked(cell) =>
      updateGrid(grid, x, y, cell)
    case cell: Cell =>
      updateGrid(grid, x, y, Marked(cell))
  }
}

private def totalOpened(grid: Grid): Int = {
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

private def nonMineCells(grid: Grid): Int = {
  grid.cells.map(
    arr => arr.count {
      case Mijn(_) => false
      case _ => true
    }
  ).sum
}

private def isGameWon(grid: Grid): Boolean = {
  val totalOpen = totalOpened(grid)
  totalOpen == nonMineCells(grid)
}

private def isGameLost(grid: Grid): Boolean = {
  grid.cells.exists(row => row.contains(Mijn(true)))
}


private def open(grid: Grid, x: Int, y: Int): Grid = {
  if (NotInbounds(grid, x, y))
    return grid

  grid.cells(y)(x) match {
    case Mijn(o) if !o =>
      updateGrid(grid, x, y, Mijn(true))
    case Leeg(o) if !o =>
      val cellSurroundingMines: List[(Int, Int)] = numSurroundingMines(grid, x, y)

      // if no mines, continue opening cells using DFS
      if (cellSurroundingMines.isEmpty) {
        val newGrid = updateGrid(grid, x, y, Leeg(true))

        val l_coords: List[(Int, Int)] = createCoordsSurroundingCells(
          newGrid,
          (x - 1 to x + 1).toList,
          (y - 1 to y + 1).toList,
          (x, y)
        )

        // Fold over the coordinates to accumulate the updated grid
        l_coords.foldLeft(newGrid) {
          (accGrid, coord) => 
            val (xb, yb) = coord
            open(accGrid, xb, yb)
        }
      } else { // else there are surrounding mines, so just update to Nummer to show amount of mines present.
        updateGrid(grid, x, y, Nummer(cellSurroundingMines.size))
      }

    case _ => grid
  }
}

private def numSurroundingMines(grid: Grid, x: Int, y: Int): List[(Int, Int)] = {
  val surroundingCoords = createCoordsSurroundingCells(
    grid,
    (x - 1 to x + 1).toList,
    (y - 1 to y + 1).toList,
    (x, y)
  )
  surroundingCoords.filter(
    (xb, yb) => grid.cells(yb)(xb) == Mijn(false) || grid.cells(yb)(xb) == Mijn(true)
  )
}

private def NotInbounds(grid: Grid, x: Int, y: Int): Boolean =
  (y < 0 || y >= grid.cells.length) ||
    (x < 0 || x >= grid.cells.head.length)

private def createCoordsSurroundingCells(
                                          grid: Grid,
                                          xr: List[Int],
                                          yr: List[Int],
                                          currCood: (Int, Int)
                                        ): List[(Int, Int)] = {
  val mapped: List[(Int, Int)] = xr.flatMap(x => yr.map(y => (x, y)))
  mapped.filter((x, y) => !NotInbounds(grid, x, y) && (x, y) != currCood)
}

private def updateGrid(grid: Grid, i_x: Int, i_y: Int, new_val: Cell): Grid = {
  val new_lst: List[Cell] = grid.cells(i_y).updated(i_x, new_val)
  Grid(grid.cells.take(i_y) ::: new_lst :: grid.cells.drop(i_y + 1))
}

private def gridPrint(grid: Grid, gameEnded: Boolean = false): Unit = {
  for (w <- grid.cells.head.indices)
    print("---")
  println()

  for (y <- grid.cells.indices) {
    print("| ")

    for (x <- grid.cells.head.indices) {
      val currCell = grid.cells(y)(x)
      currCell match {
        case Mijn(open) =>
          if (open) print(" \u1F4A  | ") else
            if (gameEnded) print("  \u1F4A   | ") else print(s" ${(x, y)}   | ")
        case Leeg(open) => if (open) print("     | ") else print(s" ${(x, y)}    | ")
        case Nummer(n) => print(s"$n     | ")
        case Marked(_) => print("F    | ")
      }
    }

    println()
  }

  for (w <- grid.cells.head.indices)
    print("---")

  println()
}

private def initGrid(width: Int, height: Int, coordsMines: List[(Int, Int)]): Grid = {
  Grid(
    (
      for (h <- 0 until height) yield 
      (
        for (w <- 0 until width) yield
          if (coordsMines.contains((w,h)))
            Mijn(false)
          else
            Leeg(false)
      ).toList
    ).toList
  )
}

def initGridRandMines(
  width: Int,
  height: Int, 
  amountMines: Int, 
  randSeed: Option[Int]
): Grid = {
  assert(
    amountMines < width * height, 
    s"Amount of mines to be created, $amountMines, exceeds total amount of cells in the grid (${width*height})!"
  )
  assert (
    amountMines > 0,
    s"Given amount of mines cannot be less or equal to 0 ;)."
  )

  val rand: Random = randSeed match {
    case Some(value) => new Random(value)
    case None => new Random
  }

  @tailrec
  def generateSetCoords(coordsSet: Set[(Int, Int)]): Set[(Int, Int)] = {
    if (coordsSet.size < amountMines) {
      val currRandomCoords: (Int, Int) = (rand.nextInt(width), rand.nextInt(height))
      generateSetCoords(coordsSet + currRandomCoords)
    } else {
      coordsSet
    }
  }

  initGrid(
    width,
    height,
    generateSetCoords(Set.empty[(Int, Int)]).toList
  )
}

@main
def main(): Unit = {

  val VALID_ACTIONS_STR: List[String] = List(
    "o",
    "m"
  )

  val initGridVals = StdIn.readLine(
    "Input width, height and amount of mines to insert. Use format width,height,num_mines: "
  ).split(",")

  if (initGridVals.size != 3) {
      println(s"Provide args of the following format: width,height,num_mines. " +
        s"You provided ${println(initGridVals.mkString("Array(", ", ", ")"))}. Example of expected args: 3,3,5")
      return
  }

  val width: Int = initGridVals(0).trim.toInt
  val height: Int = initGridVals(1).trim.toInt
  val amntMines: Int = initGridVals(2).trim.toInt
  val randSeed: Option[Int] = None
  
  @tailrec
  def mainGameLoop(grid: Grid): GameStatus = {
      println(s"Opened Total: ${totalOpened(grid)}")
      println(s"Amount To Open To Win: ${nonMineCells(grid)}")
      gridPrint(grid)

      val splitInput = StdIn.readLine(
        "Input coordinates in the form action{o,m},x,y including the comma: "
      ).split(",")

      if (splitInput.size != 3) {
        gridPrint(grid, gameEnded = true)
        return InvalidInput(
          s"Provide args of the following format: action,x,y. Actions possible: $VALID_ACTIONS_STR"
        )
      }

      val action: String = splitInput(0).trim
      val splitInputX: Int = splitInput(1).trim.toInt
      val splitInputY: Int = splitInput(2).trim.toInt

      action match
        case "o" =>
          val newGrid = open(grid, splitInputX, splitInputY)
          if (isGameWon(newGrid)) 
            gridPrint(newGrid, gameEnded = true)
            Won
          else if (isGameLost(newGrid))
            gridPrint(newGrid, gameEnded = true)
            Lost
          else
            mainGameLoop(
              newGrid
            )
        case "m" => 
          mainGameLoop(
            mark(grid, splitInputX, splitInputY)
          )
        case _ => 
          gridPrint(grid, gameEnded = true)
          InvalidInput(s"Invalid action provided: $action. Valid actions: $VALID_ACTIONS_STR") 
  }

  val gameStatus = mainGameLoop(
    initGridRandMines(width, height, amntMines, randSeed)
  )

  gameStatus match
    case Won => println("You have won!")
    case Lost => println("You have lost!")
    case InvalidInput(msg) => println(msg)
}