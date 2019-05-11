package pl.edu.agh.mock

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.algorithm.MockMovesController
import pl.edu.agh.mock.model.{MockCell, PredatorCell, PreyCell}
import pl.edu.agh.mock.model.parallel.MockConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

object MockMain extends LazyLogging {
  private val configPrefix = "mock"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      MockConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new MockMovesController(_)(_),
      {
        case MockCell(_) => Color.WHITE
        case PreyCell(_, _, _, _) => Color.CYAN
        case cell@PredatorCell(smell, _, _, _) => predatorColor(cell)
        case Obstacle => Color.BLUE
        case cell: SmellingCell => cellToColor(cell)
      }).start()
  }

  private def predatorColor(cell: PredatorCell): Color = {
    Color.getHSBColor(0.8f, 1f, (cell.state.getHealth()/100).toFloat)
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    if (smellValue < 0.00001) {
      val hue = 1f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.001) {
      val hue = 0.65f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.1) {
      val hue = 0.28f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else {
      val hue = 0.11f
      val saturation = 0.69f
      Color.getHSBColor(hue, saturation, brightness)
    }
  }

  private def cellToColor(cell: SmellingCell): Color = {
    //val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val maxSmell = cell.smell.map(_.map(_.value).map(x => (Math.abs(x), x.signum)).max).max
    val smellValue = maxSmell._1.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    val hue = 0.5f - 0.25f * maxSmell._2.toFloat
    val saturation = 0.69f
    Color.getHSBColor(hue, saturation, brightness)
  }

}

