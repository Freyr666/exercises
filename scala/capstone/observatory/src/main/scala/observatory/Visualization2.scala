package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val x = point.x
    val y = point.y
    val cx = 1 - point.x
    val cy = 1 - point.y
    d00 * cx * cy + d01 * cx * y + d10 * x * cy + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    def interpolate(loc: Location): Temperature = {
      val latInt = loc.lat.toInt
      val lonInt = loc.lon.toInt
      val loc00 = GridLocation(latInt, lonInt)
      val loc01 = GridLocation(latInt, lonInt + 1)
      val loc10 = GridLocation(latInt + 1, lonInt)
      val loc11 = GridLocation(latInt + 1, lonInt + 1)
      val point = CellPoint(loc.lat - latInt, loc.lon - lonInt)
      bilinearInterpolation(point, grid(loc00), grid(loc01), grid(loc10), grid(loc11))
    }

    val width = 256
    val height = 256

    val cols = for {
      yOff <- 0 until height
      xOff <- 0 until width
      subtile = tile.subtile(xOff, yOff, 8)
      loc = Interaction.tileLocation(subtile)
      temp = interpolate(loc)
    } yield Visualization.interpolateColor(colors, temp)

    val pixels: Array[Pixel] = cols
      .map(c => Pixel(c.toARGBInt))
      .toArray

    Image(width, height, pixels)
  }

}
