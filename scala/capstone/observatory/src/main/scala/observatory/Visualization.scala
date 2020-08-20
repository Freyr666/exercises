package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val P = 6
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def areAntipodes(a: Location, b: Location) =
      (a.lat == -b.lat) && (math.abs(a.lon - b.lon) == 180)

    def distance(a: Location, b: Location): Double = {
      val earthRadius = 6371.0
      val dSigma =
        if (a == b) {
          0
        } else if (areAntipodes(a, b)) {
          math.Pi
        } else {
          val dLambda = math.toRadians(math.abs(a.lon - b.lon))
          val aLat = math.toRadians(a.lat)
          val bLat = math.toRadians(b.lat)
          math.acos(
            math.sin(aLat) * math.sin(bLat) +
              math.cos(aLat) * math.cos(bLat) * math.cos(dLambda))
        }
      earthRadius * dSigma
    }

    val dists = temperatures.map { case (loc, temp) =>
      (distance(location, loc), temp)
    }

    val closest = dists min Ordering.by((x: (Double, Temperature)) => x._1)

    if (closest._1 < 1) {
      closest._2
    } else {
      val invDists = dists.map(d => (1 / math.pow(d._1, P), d._2))

      val num = invDists.foldLeft(0.0)((acc, d) => acc + (d._1 * d._2))
      val denom = invDists.foldLeft(0.0)((acc, d) => acc + d._1)
      num / denom
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val (s, b) = points.partition(_._1 <= value)

    if (s.isEmpty) (b min Ordering.by((p: (Temperature, Color)) => p._1))._2
    else if (b.isEmpty) (s max Ordering.by((p: (Temperature, Color)) => p._1))._2
    else {
      val (lt, lcol) = (s max Ordering.by((p: (Temperature, Color)) => p._1))
      val (rt, rcol) = (b min Ordering.by((p: (Temperature, Color)) => p._1))

      if (lt == value) lcol
      else {
        val dl = 1 / math.abs(lt - value)
        val dr = 1 / math.abs(rt - value)
        def interp(x: Int, y: Int): Int =
          ((x * dl + y * dr) / (dl + dr)).round.toInt

        Color(
          interp(lcol.red, rcol.red),
          interp(lcol.green, rcol.green),
          interp(lcol.blue, rcol.blue))
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180

    val cols = for {
      h <- 90 to -89 by -1
      w <- -180 to 179
      temp = predictTemperature(temperatures, Location(h, w))
    } yield (interpolateColor(colors, temp))

    val pixels: Array[Pixel] = cols
      .map(c => Pixel(c.toRGBInt))
      .toArray

    Image(width, height, pixels)
  }
}

