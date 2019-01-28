package observatory

import com.sksamuel.scrimage.Image
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val WIDTH = 360
  val HEIGHT = 180

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)],
                         location: Location): Temperature = {
    val dists = temperatures map (entry =>
      (location distance entry._1, entry._2))
    val min = dists reduce ((a, b) => if (a._1 < b._1) a else b)
    if (min._1 <= 1) {
      min._2
    } else {
      /*val weights = dists.map(entry => (1 / pow(entry._1, 2), entry._2))
      val normalizer = weights.map(_._1).sum
      weights.map(entry => entry._1 * entry._2).sum  / normalizer*/
      val (norm, acc) = dists.foldLeft((0.0, 0.0))((B, C) =>
        (B._1 + (1 / pow(C._1, 2)), B._2 + C._2 / pow(C._1, 2)))
      acc / norm
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)],
                       value: Temperature): Color = {
    points find (_._1 == value) match {
      case Some((_, color)) => color
      case None =>
        val (smaller, greater) =
          points.toList.sortBy(_._1).partition(_._1 < value)
        linerHelper(smaller.reverse.headOption, greater.headOption, value)

    }
  }
  /*def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sorted = points.toSeq.sortBy(_._1)
    if (sorted.head._1 >= value) return sorted.head._2
    if (sorted.last._1 <= value) return sorted.last._2

    val (left: (Temperature, Color), right: (Temperature, Color)) = sorted.sliding(2).find {
      t => t(0)._1 <= value && t(1)._1 >= value
    } match {
      case Some(p: Seq[(Temperature, Color)]) => (p(0), p(1))
      case None => (sorted.head, sorted.last)
    }
    doInterpolate(left, right, value)
  }

  def doInterpolate(left: (Temperature, Color), right: (Temperature, Color), value: Temperature): Color = {
    if (left.equals(right) || left._1.equals(value)) return left._2
    if (right._1.equals(value)) return right._2

    val alpha: Double = (value - left._1) / (right._1 - left._1)
    val red = alpha * (right._2.red - left._2.red) + left._2.red
    val green = alpha * (right._2.green - left._2.green) + left._2.green
    val blue = alpha * (right._2.blue - left._2.blue) + left._2.blue
    Color(red.toInt, green.toInt, blue.toInt)
  }*/
  private def linerHelper(pointA: Option[(Temperature, Color)],
                          pointB: Option[(Temperature, Color)],
                          values: Temperature): Color =
    (pointA, pointB) match {
      case (Some(pA), Some(pB)) =>
        val li = linearInterpolationValue(pA._1, pB._1, values) _
        Color(
          li(pA._2.red, pB._2.red),
          li(pA._2.green, pB._2.green),
          li(pA._2.blue, pB._2.blue)
        )
      case (Some(pA), None) => pA._2
      case (None, Some(pB)) => pB._2
      case _                => Color(0, 0, 0)
    }
  def linearInterpolationValue(
      pointValueMin: Double,
      pointValueMax: Double,
      value: Double)(colorValueMin: Int, colorValueMax: Int): Int = {
    val factor = (value - pointValueMin) / (pointValueMax - pointValueMin)

    round(colorValueMin + (colorValueMax - colorValueMin) * factor).toInt
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)],
                colors: Iterable[(Temperature, Color)]): Image = {
    val locationMap = posToLocation(WIDTH, HEIGHT) _
    val pixels = (0 until HEIGHT * WIDTH).par
      .map { pos =>
        pos -> interpolateColor(colors,
                                predictTemperature(temperatures,
                                                   locationMap(pos))).pixel()
      }
      .seq
      .sortBy(_._1)
      .map(_._2)
    Image(WIDTH, HEIGHT, pixels.toArray)
  }
  def posToLocation(imageWidth: Int, imageHeight: Int)(pos: Int): Location = {
    val widthFactor = 180 * 2 / imageWidth.toDouble
    val heightFactor = 90 * 2 / imageHeight.toDouble

    val x: Int = pos % imageWidth
    val y: Int = pos / imageWidth

    Location(90 - (y * heightFactor), (x * widthFactor) - 180)
  }
}
