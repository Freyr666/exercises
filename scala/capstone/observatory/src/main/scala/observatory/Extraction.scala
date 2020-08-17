package observatory

import java.time.LocalDate
import scala.io.Source

import org.apache.spark.sql.SparkSession
import scala.collection.script.Location

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.executor.memory", "1G")
      .config("spark.master", "local")
      .getOrCreate()

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    def lines(s: String): Seq[String] =
      Source.fromInputStream(Source.getClass.getResourceAsStream(s))
        .getLines()
        .toSeq

    val stationsRdd = spark.sparkContext.makeRDD(lines(stationsFile))
    val tempsRdd = spark.sparkContext.makeRDD(lines(temperaturesFile))

    val stations = stationsRdd
      .map(_.split(","))
      .filter(_.length == 4)
      .map(a => ((a(0), a(1)), Location(a(2).toDouble, a(3).toDouble)))

    val temps = tempsRdd
      .map(_.split(","))
      .filter(_.length == 5)
      .map(a =>
        ((a(0), a(1)), (LocalDate.of(year, a(2).toInt, a(3).toInt), parseTemp(a(4)))))

    stations.join(temps)
      .mapValues(v => (v._2._1, v._1, v._2._2))
      .values
      .collect()
      .toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val recordsRdd = spark.sparkContext.parallelize(records.toSeq)

    recordsRdd
      .groupBy(_._2)
      .mapValues { l =>
        val sum = l.foldLeft(0.0){ (acc, v) =>
          acc + v._3
        }
        sum / l.size
      }
      .collect()
      .toSeq
  }

  def parseTemp(s: String): Temperature = {
    val f = s.toDouble
    (f - 32) / 1.8
  }
}
