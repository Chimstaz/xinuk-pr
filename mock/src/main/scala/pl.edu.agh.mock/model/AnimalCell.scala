package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

/* This cell type keep position within cell which allow to make smaller moves */

abstract class AnimalCell(smell: SmellArray, x: Double, y: Double, state: AnimalState) extends SmellingCell {
  def move(moveX: Double, moveY: Double): (Double, Double, Int, Int) = {
    val newX = moveX + x
    val newY = moveY + y

    (
      if (Math.abs(newX) > AnimalCell.passValue) if (newX < 0) (newX + AnimalCell.passValue) * (-1) else (newX - AnimalCell.passValue) else newX,
      if (Math.abs(newY) > AnimalCell.passValue) if (newY < 0) (newY + AnimalCell.passValue) * (-1) else (newY - AnimalCell.passValue) else newY,
      if (Math.abs(newX) > AnimalCell.passValue) if (newX < 0) -1 else 1 else 0,
      if (Math.abs(newY) > AnimalCell.passValue) if (newY < 0) -1 else 1 else 0)
  }

}

object AnimalCell {
  /* Values in range (-passValue; passValue) are inside one cell */
  val passValue = 10
}


case class AnimalState(energy: Double, health: Int, regenerationRate: Double, eatingEfficiency: Double) {
  def regenerate(usedEnergy: Double, ate: Double): AnimalState = {
    val regeneratedEnergy = health match {
      case x if x > 80 => regenerationRate
      case x if x > 20 => regenerationRate * 0.8 * (x - 20) / 60 + regenerationRate * 0.2
      case _ => regenerationRate * 0.2
    }

    AnimalState(Math.min(10, energy - usedEnergy + regeneratedEnergy), Math.min(100, health + ate * eatingEfficiency).toInt, regenerationRate, eatingEfficiency)
  }
}

final case class MoveVector(xInsideCell: Double, yInsideCell: Double, xCell: Int, yCell: Int)