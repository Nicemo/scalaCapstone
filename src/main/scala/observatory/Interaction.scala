package observatory
import observatory.Visualization._
import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val IMG_WIDTH, IMG_HEIGHT = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.toLocation
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tileAllLocations(tile: Tile): Array[Location] = {

    val locationsMap = new Array[Location](IMG_WIDTH * IMG_HEIGHT)
    for {
      y <- 0 until IMG_HEIGHT
      x <- 0 until IMG_WIDTH
    } {
      locationsMap(y * IMG_HEIGHT + x) = Tile(IMG_WIDTH * tile.x + x, IMG_HEIGHT * tile.y, tile.zoom).toLocation
    }
    locationsMap
  }

  def tile(temperatures: Iterable[(Location, Temperature)],
           colors: Iterable[(Temperature, Color)],
           tile: Tile): Image = {
    val tmps = tileAllLocations(tile).map { l=> predictTemperature(temperatures, l)}
    val pixels = tmps.map{t => interpolateColor(colors, t)}
    val img = pixels.map {p => p.pixel(127)
    }
    Image(IMG_WIDTH, IMG_HEIGHT, img)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
      yearlyData: Iterable[(Year, Data)],
      generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } {
      generateImage(year, Tile(x, y, zoom), data)
    }
  }

}
