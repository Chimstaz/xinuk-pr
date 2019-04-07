package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

/* This cell type keep position within cell which allow to make smaller moves */

abstract class ContinuousPosCell(smell: SmellArray, x: Double, y: Double) extends SmellingCell {
  def move(moveX: Double, moveY: Double): (Double, Double, Int, Int) = {
    val newX = moveX + x
    val newY = moveY + y

    if (newX > ContinuousPosCell.passValue) {

    }
    (
      if (Math.abs(newX) > ContinuousPosCell.passValue) if (newX < 0) (newX + ContinuousPosCell.passValue) * (-1) else (newX - ContinuousPosCell.passValue) else newX,
      if (Math.abs(newY) > ContinuousPosCell.passValue) if (newY < 0) (newY + ContinuousPosCell.passValue) * (-1) else (newY - ContinuousPosCell.passValue) else newY,
      if (Math.abs(newX) > ContinuousPosCell.passValue) if (newX < 0) -1 else 1 else 0,
      if (Math.abs(newY) > ContinuousPosCell.passValue) if (newY < 0) -1 else 1 else 0)
  }
}

object ContinuousPosCell {
  val passValue = 10
}
