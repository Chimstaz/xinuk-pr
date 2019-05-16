package pl.edu.agh.mock.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class MockMetrics(predatorCount : Long,
                             preyCount : Long,
                            ) extends Metrics {
  override def log: String = {
    s"$predatorCount;$preyCount;"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Predators" -> predatorCount,
    "Preys" -> preyCount
  )

  override def +(other: Metrics): MockMetrics = {
    other match {
      case MockMetrics.EMPTY => this
      case MockMetrics(otherPredatorCount, otherPreyCount) =>
        MockMetrics(predatorCount + otherPredatorCount, preyCount + otherPreyCount)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-MockMetrics to MockMetrics")
    }
  }
}

object MockMetrics {
  private val EMPTY = MockMetrics(0, 0)

  def empty(): MockMetrics = EMPTY
}