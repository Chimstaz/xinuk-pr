package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, GridPart, Signal, SmellingCell}

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

  def makeMove(neighbours: Iterator[(Int, Int, GridPart)]): Iterator[(Int, Int, GridPart)]

}

object AnimalCell {
  /* Values in range (-passValue; passValue) are inside one cell */
  val passValue = 10
}


class AnimalState(energy: Double, health: Double, regenerationRate: Double, eatingEfficiency: Double, healthLose: Double) {
  def getEnergy() = energy
  def getHealth() = health

  def regenerate(usedEnergy: Double, ate: Double): AnimalState = {
    new AnimalState(Math.min(Math.max(20, energy), energy - usedEnergy + regenerateEnergy()), Math.min(100, health + regenerateHealth(ate)) - healthLose, regenerationRate, eatingEfficiency, healthLose)
  }

  def regenerateEnergy(): Double = health match {
    case x if x > 80 => regenerationRate
    case x if x > 20 => regenerationRate * 0.8 * (x - 20) / 60 + regenerationRate * 0.2
    case _ => regenerationRate * 0.2
  }

  def regenerateHealth(ate: Double) = ate * eatingEfficiency
}

final case class MoveVector(xInsideCell: Double, yInsideCell: Double, xCell: Int, yCell: Int)