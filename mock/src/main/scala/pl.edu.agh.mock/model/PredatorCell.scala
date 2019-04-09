package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class PredatorCell(smell: SmellArray, x: Double, y: Double, state: AnimalState) extends AnimalCell(smell, x, y, state) {
  override type Self = PredatorCell

  override def withSmell(smell: SmellArray): PredatorCell = copy(smell = smell)
}

object PredatorCell {

  def create(initialSignal: Signal, x: Double, y: Double, state: AnimalState): PredatorCell = PredatorCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), x, y, state)

  def create(initialSignal: Signal): PredatorCell = PredatorCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), 0, 0, AnimalState(10, 100, 1, 1))
}
