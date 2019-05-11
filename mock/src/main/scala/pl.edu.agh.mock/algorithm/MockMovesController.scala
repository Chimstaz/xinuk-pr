package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.parallel.MockConflictResolver
import pl.edu.agh.mock.model.{PredatorCell, PreyCell, _}
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone)

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = PreyCell.create(config.mockInitialSignal)

    grid.cells(config.gridSize / 4 + 20)(config.gridSize / 4 + 20) = PredatorCell.create(config.mockInitialSignal)


    val metrics = MockMetrics.empty()
    (grid, metrics)
  }

  def getNeighbour(x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    Grid.neighbourCellCoordinates(x, y).iterator.map(cord => (cord._1 - x, cord._2 - y, grid.cells(cord._1)(cord._2)))
  }

  def getNeighbourBySmell(cell: AnimalCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i - x, j - y, grid.cells(i)(j))
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
        case predator@PredatorCell(_,_,_,_) => moveAnimalCells(x, y, predator)
        case prey@PreyCell(_,_,_,_) => moveAnimalCells(x, y, prey)
      }
    }

    def moveAnimalCells(x: Int, y: Int, cell: AnimalCell): Unit = {
      val move2 = cell.makeMove(getNeighbourBySmell(cell, x, y, grid))
      move2.foreach(t => {
        val smellingT = t._3 match {
          case BufferCell(bufCell) => bufCell
          case _ => t._3
        }
        val smellingGrid = newGrid.cells(x + t._1)(y + t._2) match {
          case BufferCell(bufCell) => bufCell
          case _ => newGrid.cells(x + t._1)(y + t._2)
        }
        val smellingRes = (smellingGrid, smellingT) match {
          case (sg: SmellingCell, st: SmellingCell) =>
            MockConflictResolver.resolveConflict(sg, st)._1
          case _ =>
            smellingT
        }
        t._3 match {
          case BufferCell(_) =>
            newGrid.cells(x + t._1)(y + t._2) = smellingRes match {
              case smelling: SmellingCell => BufferCell(smelling)
              case _ => t._3
            }
          case _ =>
            newGrid.cells(x + t._1)(y + t._2) = smellingRes
        }
      })

    }

    def movePredatorCells(x: Int, y: Int, cell: PredatorCell): Unit = {
      val move2 = cell.makeMove(getNeighbourBySmell(cell, x, y, grid))
      move2.foreach(t => newGrid.cells(x + t._1)(y + t._2) = t._3)
    }

    def movePreyCells(x: Int, y: Int, cell: PreyCell): Unit = {
      val move2 = cell.makeMove(getNeighbourBySmell(cell, x, y, grid))
      move2.foreach(t => newGrid.cells(x + t._1)(y + t._2) = t._3)
    }


    def moveMockCells(x: Int, y: Int, cell: GridPart): Unit = {

      val destination = if (random.nextInt(2) == 0) {(x + random.nextInt(3) - 1, y + random.nextInt(3) - 1) } else {(0, 0)}
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
      case (_, _, PredatorCell(_, _, _, _)) => true
      case (_, _, PreyCell(_, _, _, _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MockMetrics.empty())
  }
}