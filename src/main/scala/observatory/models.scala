package observatory
import scala.math._
import com.sksamuel.scrimage.RGBColor

/**
  * Introduced in Week 1. Represents a location on the globe.
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  val EARTH_RADIUS = 6371
  def areAntipodes(that: Location): Boolean = {
    (this.lat == -that.lat) && (abs(this.lon - that.lon) == 180)
  }

  def distance(that: Location): Int =
    that match {
      case same if same == this                 => 0
      case antiPodes if areAntipodes(antiPodes) => (EARTH_RADIUS * Pi).toInt
      case other                                => greatCircleDist(other)
    }
  def greatCircleDist(that: Location): Int = {
    val deltaLat = toRadians(this.lat - that.lat)
    val deltaLon = toRadians(this.lon - that.lon)
    val a = sin(deltaLat / 2) * sin(deltaLat / 2) + (cos(toRadians(this.lat)) *
      cos(toRadians(that.lat)) * sin(deltaLon / 2) * sin(deltaLon / 2))
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
    (EARTH_RADIUS * c).toInt
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {
  val n = pow(2, zoom)
  val lng = x / n * 360.0 - 180.0
  val lat = ((atan(sinh(Pi * (1.0 - 2.0 * y / n))).toDegrees) % 180.0)
  val toLocation = Location(lat, lng)

  def toURI =
    new java.net.URI(
      "http://tile.openstreetmap.org/" + zoom + "/" + x + "/" + y + ".png")
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int) {
  def toLocation = Location(lat, lon)
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {

  def pixel(alpha: Int = 255) = RGBColor(red, green, blue, alpha).toPixel

}

case class Station(stn: STN, wban: WBAN)
object Convert {
  def fToC(d: Double) = round((d - 32) * 5 / 9)
  private def round(d: Double) =
    BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}
