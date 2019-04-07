package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class PredatorCell(smell: SmellArray, x: Double, y: Double) extends ContinuousPosCell(smell, x, y) {
  override type Self = PredatorCell

  override def withSmell(smell: SmellArray): PredatorCell = copy(smell = smell)
}

object PredatorCell {

  def create(initialSignal: Signal, x: Double, y: Double): PredatorCell = PredatorCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), x, y)

  def create(initialSignal: Signal): PredatorCell = PredatorCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), 0, 0)
}
