package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val z: Double = 1 << tile.zoom
    val lon = (tile.x / z) * 360 - 180
    val latRad = math.atan(math.sinh(math.Pi * (1 - 2 * tile.y / z)))
    val lat = math.toDegrees(latRad)
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val (width, height) = (256, 256)
    val cols = for {
      yOff <- 0 until height
      xOff <- 0 until width
      subtile = tile.subtile(xOff, yOff, 8)
      location = tileLocation(subtile)
      temp = Visualization.predictTemperature(temperatures, location)
    } yield Visualization.interpolateColor(colors, temp)

    val pixels: Array[Pixel] = cols
      .map(c => Pixel(c.toARGBInt))
      .toArray

    Image(width, height, pixels)
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
    val params: Seq[(Int, Int, Int)] =
      for {
        zoom <- 0 to 3
        y <- 0 until (1 << zoom)
        x <- 0 until (1 << zoom)
      } yield((zoom, x, y))

    params
      .par
      .foreach { case (zoom, x, y) =>
        val tile = Tile(x, y, zoom)
        println(s"Processing $tile")
        yearlyData.foreach(p => generateImage(p._1, tile, p._2))
      }
  }

}
