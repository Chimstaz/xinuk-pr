package pl.edu.agh.mock.model

import javax.print.attribute.standard.Destination
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class PredatorCell(smell: SmellArray, x: Double, y: Double, state: PredatorState) extends AnimalCell(smell, x, y, state) {
  override type Self = PredatorCell

  override def withSmell(smell: SmellArray): PredatorCell = copy(smell = smell)

  def calculateSpeedToPray(loud: Double): (Double, PredatorAction.Value) = {
    if (state.energy < 0) {
      return (0, PredatorAction.Walk)
    }
    //println(state.energy, state.health, loud)
    //println(loud, calculateSmell(state.speed)(1)(1).value, loud - calculateSmell(state.speed)(1)(1).value/10000)
    loud match {
      case l if l <= 1E-30 => (0, state.action)
      //case x if x < 3 => x * x * 6
      case l if l < 1E-20 => (8, state.action)
      case l if l < 1E-15 => (6, state.action)
      case l if l < 1E-10 => (7, state.action)
      case l if l < 1E-7 => (4, state.action)
      case l if l < 1E-5 => (0.2, state.action)
      case l if l < 1E-4 => (0.1, state.action)
      case _ => if (state.energy > 10 || state.action == PredatorAction.Run) {
        (10, PredatorAction.Run)
      } else {
        (0.05, state.action)
      }
    }
  }

  def calculateSpeedInDirection(dir: Int, speed: Double, lastSpeed: Double) = (dir * speed + lastSpeed * 0.1)/1.1

  def calculateSmellVal(destination: (Double, Double)) =
    -Math.max(destination._1*destination._1, destination._2*destination._2)/100

  def calculateSmell(destination: (Double, Double)) =
    Array.fill(Cell.Size, Cell.Size)(Signal(calculateSmellVal(destination)))

  override def makeMove(neighbours: Iterator[(Int, Int, GridPart)]): Iterator[(Int, Int, GridPart)] = {
    val loudest = neighbours.next()
    val speed = calculateSpeedToPray(smell(loudest._1 + 1)(loudest._2 + 1).value)
    val destination = (calculateSpeedInDirection(loudest._1, speed._1, state.speed._1), calculateSpeedInDirection(loudest._2, speed._1, state.speed._2))

    val it = List.newBuilder[(Int, Int, GridPart)]

    val vacatedCell = EmptyCell(smell)

    val m = move(destination._1, destination._2)
    val occupiedCell = copy(calculateSmell(destination), m._1, m._2, state.update(-calculateSmellVal(destination), 1, destination, speed._2))
    //PredatorCell.create(Signal(-Math.sqrt(destination._1*destination._1 + destination._2*destination._2)/100), m._1, m._2, state)

    if (m._3 == 0 && m._4 == 0) {
      it += ((0, 0, occupiedCell))
    } else {
      (Iterator.single(loudest) ++ neighbours).find(p => p._1 == m._3 && p._2 == m._4).get._3 match {
        case EmptyCell(_)|PreyCell(_,_,_,_) =>
          it += ((0, 0, vacatedCell))
          it += ((m._3, m._4, occupiedCell))
        case BufferCell(_) =>
          it += ((0, 0, vacatedCell))
          it += ((m._3, m._4, BufferCell(occupiedCell)))
        case _ =>
          it += ((0, 0, occupiedCell))
      }
    }

    it.result().iterator
  }
}

object PredatorCell {

  def create(initialSignal: Signal, x: Double, y: Double, state: PredatorState): PredatorCell = PredatorCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), x, y, state)

  def create(initialSignal: Signal): PredatorCell = PredatorCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), 0, 0, PredatorState(10, 100, (0, 0), PredatorAction.Walk))
}

object PredatorAction extends Enumeration {
  val Walk, Run = Value
}

case class PredatorState(energy: Double, health: Int, speed: (Double, Double), action: PredatorAction.Value) extends AnimalState(energy, health, 0.1, 1, 1) {
  def update(usedEnergy: Double, ate: Double, speed: (Double, Double), action: PredatorAction.Value): PredatorState = {
    val newAnimalState = super.regenerate(usedEnergy, ate)
    copy(energy = newAnimalState.getEnergy(), health = newAnimalState.getHealth(), speed = speed, action = action)
  }

  def changeSpeed(speed: (Double, Double)) = {
    copy(speed = speed)
  }
}