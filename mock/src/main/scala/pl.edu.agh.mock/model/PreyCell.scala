package pl.edu.agh.mock.model

import org.slf4j.LoggerFactory
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

import scala.util.Random

final case class PreyCell(smell: SmellArray, x: Double, y: Double, state: PreyState) extends AnimalCell(smell, x, y, state) {
  override type Self = PreyCell

  def getRandomMove(): Double = {
    Math.min(PreyCell.random.nextDouble() * 2, 10) * (if(Random.nextInt(2) == 0) {1} else {-1})
  }

  def calculateSmellVal(destination: (Double, Double)) =
    Math.max(destination._1*destination._1, destination._2*destination._2)/50

  def calculateSmell(destination: (Double, Double)) =
    Array.fill(Cell.Size, Cell.Size)(Signal(calculateSmellVal(destination)))

  def getNewCell(action: PreyAction.Value, destination: (Double, Double), m: (Double, Double), eat: Double, energyUsed: Double): PreyCell = {
    copy(
      if (action == PreyAction.Eat) {
        Array.fill(Cell.Size, Cell.Size)(Signal(5))
      } else {
        calculateSmell(destination)
      },
      m._1, m._2,
      state.regenerate(energyUsed, eat).changeAction(action))
  }

  override def makeMove(neighbours: Iterator[(Int, Int, GridPart)]): Iterator[(Int, Int, GridPart)] = {
    val neighboursList = neighbours.toList
    val loudest = neighboursList.last
    val it = List.newBuilder[(Int, Int, GridPart)]

    val (destination, eat, action) =
      if (smell(loudest._1 + 1)(loudest._2 + 1).value < -0.00005 || (state.health > 50 && state.action == PreyAction.Run)) {
        // In danger, run away
        ((-8.0 * loudest._1, -8.0 * loudest._2), 0, PreyAction.Run)
      } else {
        if (state.getHealth() > 90 || (state.getHealth() > 50 && state.action == PreyAction.Walk)) {
          if (PreyCell.random.nextInt(4) == 0) {
            // in direction of Prey
            ((Math.abs(getRandomMove()) * neighboursList.head._1, Math.abs(getRandomMove()) * neighboursList.head._2), 0, PreyAction.Walk)
          }
          else {
            // random move
            ((getRandomMove(), getRandomMove()), 0, PreyAction.Walk)
          }
        } else {
          // eat
          ((0.0, 0.0), 5, PreyAction.Eat)
        }
      }

    val m = move(destination._1, destination._2)
    val vacatedCell = EmptyCell(smell)
    val occupiedCell = (additionalEnergyUsed: Double, p: PreyCell) => p.copy(
      if (action == PreyAction.Eat) {
        Array.fill(Cell.Size, Cell.Size)(Signal(5))
      } else {
        calculateSmell(destination)
      },
      m._1, m._2,
      state.regenerate(calculateSmellVal(destination) + additionalEnergyUsed, eat).changeAction(action))

    val logger = LoggerFactory.getLogger("aa")
    if (m._3 == 0 && m._4 == 0) {
      it += ((0, 0, occupiedCell(0, this)))
    } else {
      neighboursList.find(p => p._1 == m._3 && p._2 == m._4).get._3 match {
        case predator@PredatorCell(_, _, _, _) =>
          // Don't mind the logic with it below, the important thing is
          // that this case never gets called and I'm not sure why
          logger.info(s"Prey about to step on Predator.")
          it += ((0, 0, vacatedCell))
          it += ((m._3, m._4, predator))
        case EmptyCell(_) =>
          if (state.energy > 19 && PreyCell.random.nextInt(5) == 0) {
            // Give birth
            it += ((0, 0, PreyCell(smell, m._3 * 10, m._4 * 10, PreyState(state.energy/2, state.health, state.action))))
            it += ((m._3, m._4, occupiedCell(state.energy/2, this)))
          } else {
            it += ((0, 0, vacatedCell))
            it += ((m._3, m._4, occupiedCell(0, this)))
          }
        case BufferCell(_) =>
          it += ((0, 0, vacatedCell))
          it += ((m._3, m._4, BufferCell(occupiedCell(0, this))))
        case _ =>
          it += ((0, 0, occupiedCell(0, this)))
      }
    }

    it.result().iterator
  }

  override def withSmell(smell: SmellArray): PreyCell = copy(smell = smell)
}

object PreyCell {
  private val random = new Random(System.nanoTime())

  def create(initialSignal: Signal): PreyCell = PreyCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), 0, 0, PreyState(10, 100, PreyAction.Walk))
}

object PreyAction extends Enumeration {
  val Eat, Walk, Run = Value
}

case class PreyState(energy: Double, health: Int, action: PreyAction.Value) extends AnimalState(energy, health, 1, 1, 1) {
  override def regenerate(usedEnergy: Double, ate: Double) = {
    val newAnimalState = super.regenerate(usedEnergy, ate)
    copy(energy = newAnimalState.getEnergy(), health = newAnimalState.getHealth())
  }

  def changeAction(action: PreyAction.Value) = {
    copy(action = action)
  }
}