package observatory

import java.io.File
import java.nio.file.{Paths, Files}

import org.apache.log4j.{Level, Logger}

object Main extends App {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  deviationTiles()

  def deviationTiles() = {

    val colors: List[(Temperature, observatory.Color)] = List(
      (7, observatory.Color(0,0,0)),
      (4, observatory.Color(255,0,0)),
      (2, observatory.Color(255,255,0)),
      (0, observatory.Color(255,255,255)),
      (-2, observatory.Color(0,255,255)),
      (-7, observatory.Color(0,0,255)))

    println("Extracting data for years 1975 - 1990")
    val data = for {
      year <- 1975 to 1990
      //temps = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
      avg = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv"))
    } yield(avg)
    println("Extraction complete!")

    println("Evaluating normals")
    val normals = Manipulation.average(data)
    println("Evaluation complete")

    println("Extracting data for years 1991 - 2015")
    val yearlyData: Iterable[(Year, Iterable[(Location, Temperature)])] =
      for {
        year <- 1991 to 2015
        //temps = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
        avg = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv"))
      } yield((year, avg))
    println("Extraction complete")

    def generateImage(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]): Unit = {
      val zoom = tile.zoom
      val x = tile.x
      val y = tile.y
      val dir = f"target/deviations/$year%d/$zoom%d"
      val fn = f"$dir%s/$x%d-$y%d.png"
      Files.createDirectories(Paths.get(dir))

      val grid = Manipulation.deviation(data, normals)
      val img = Visualization2.visualizeGrid(grid, colors, tile)
      img.output(new File(fn))
    }

    Interaction.generateTiles(yearlyData, generateImage)
  }

  def annualTiles() = {
    val colors: List[(Temperature, observatory.Color)] = List(
      (60, observatory.Color(255, 255, 255)),
      (32, observatory.Color(255, 0, 0)),
      (12, observatory.Color(255, 255, 0)),
      (0, observatory.Color(0, 255, 255)),
      (-15, observatory.Color(0, 0, 255)),
      (-27, observatory.Color(255, 0, 255)),
      (-50, observatory.Color(33, 0, 107)),
      (-60, observatory.Color(0, 0, 0)))

    val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
    val avg = Extraction.locationYearlyAverageRecords(temps)

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

}
