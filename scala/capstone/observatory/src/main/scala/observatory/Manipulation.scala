package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val m = new collection.concurrent.TrieMap[GridLocation, Temperature] //new collection.mutable.HashMap[GridLocation, Temperature]

    { loc =>
      m.get(loc) match {
        case Some(v) => v
        case None => {
          val pred = Visualization.predictTemperature(temperatures, loc.location)
          m(loc) = pred
          pred
        }
      }

    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val m = new collection.concurrent.TrieMap[GridLocation, Temperature]
    val grids: Iterable[GridLocation => Temperature] = temperaturess.map(makeGrid(_))

    { loc =>
      m.get(loc) match {
        case Some(v) => v
        case None => {
          val avg = grids.map(f => f(loc)).sum / grids.size
          m(loc) = avg
          avg
        }
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)

    loc => grid(loc) - normals(loc)
  }


}

