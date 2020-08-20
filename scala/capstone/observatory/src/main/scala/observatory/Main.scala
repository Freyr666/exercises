package observatory

import java.io.File
import java.nio.file.{Paths, Files}

import org.apache.log4j.{Level, Logger}

object Main extends App {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
  val avg = Extraction.locationYearlyAverageRecords(temps)

  val colors: List[(Temperature, observatory.Color)] = List(
    (60, observatory.Color(255, 255, 255)),
    (32, observatory.Color(255, 0, 0)),
    (12, observatory.Color(255, 255, 0)),
    (0, observatory.Color(0, 255, 255)),
    (-15, observatory.Color(0, 0, 255)),
    (-27, observatory.Color(255, 0, 255)),
    (-50, observatory.Color(33, 0, 107)),
    (-60, observatory.Color(0, 0, 0)))

  val yearlyData: Iterable[(Year, Iterable[(Location, Temperature)])] = List((2015, avg))

  def generateImage(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]): Unit = {
    val zoom = tile.zoom
    val x = tile.x
    val y = tile.y
    val dir = f"target/temperatures/$year%d/$zoom%d"
    val fn = f"$dir%s/$x%d-$y%d.png"
    Files.createDirectories(Paths.get(dir))

    val img = Interaction.tile(data, colors, tile)
    img.output(new File(fn))
  }

  Interaction.generateTiles(yearlyData, generateImage)

}
