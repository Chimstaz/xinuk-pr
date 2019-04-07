package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone)

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal)

    grid.cells(config.gridSize / 4 + 2)(config.gridSize / 4 + 2) = PredatorCell.create(config.mockInitialSignal)


    val metrics = MockMetrics.empty()
    (grid, metrics)
  }

  def calculatePredatorDirection(cell: PredatorCell, x: Int, y: Int, grid: Grid): Unit = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
      cell match {
        case MockCell(_) => moveMockCells(x, y, cell)
        case predator@PredatorCell(_,_,_) => movePredatorCells(x, y, predator)
      }
    }

    def movePredatorCells(x: Int, y: Int, cell: PredatorCell): Unit = {

      val destination = if (random.nextInt(8) == 0) {(random.nextDouble() * 10 - 5, random.nextDouble() * 10 - 5)} else (0.1, 0.1)
      val vacatedCell = EmptyCell(cell.smell)

      val move = cell.move(destination._1, destination._2)

      val occupiedCell = PredatorCell.create(Signal(Math.sqrt(destination._1*destination._1 + destination._2*destination._2)), move._1, move._2)

      if (move._3 == 0 && move._4 == 0) {
        newGrid.cells(x)(y) = occupiedCell
      } else {
        newGrid.cells(x + move._3)(y + move._4) match {
          case EmptyCell(_) =>
            newGrid.cells(x)(y) = vacatedCell
            newGrid.cells(x + move._3)(y + move._4) = occupiedCell
          case BufferCell(EmptyCell(_)) =>
            newGrid.cells(x)(y) = vacatedCell
            newGrid.cells(x + move._3)(y + move._4) = BufferCell(occupiedCell)
          case _ =>
            newGrid.cells(x)(y) = PredatorCell.create(Signal.Zero, cell.x, cell.y)
        }
      }
    }


    def moveMockCells(x: Int, y: Int, cell: GridPart): Unit = {

      val destination = if (random.nextInt(8) == 0) {(x + random.nextInt(3) - 1, y + random.nextInt(3) - 1) } else {(0, 0)}
      val vacatedCell = EmptyCell(cell.smell)
      val occupiedCell =

      if (destination._1 == 0 && destination._2 == 0) {
        MockCell.create(Signal.Zero)
      } else {
        MockCell.create(config.mockInitialSignal)
      }

      newGrid.cells(destination._1)(destination._2) match {
        case EmptyCell(_) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = occupiedCell
        case BufferCell(EmptyCell(_)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MockCell(_)) => true
      case (_, _, PredatorCell(_, _, _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MockMetrics.empty())
  }
}