package com.farrellw.hwe.practice

case class Item(description: String, price: Option[Int])

case class WeatherStation(name: String, temperature: Option[Int])

object OptionEither {
  /*
    Returns age of a dog when given a human age.
    Returns None if the input is None.
  */
  def dogAge(humanAge: Option[Int]): Option[Int] = {
    if (humanAge.isDefined) Some(humanAge.get * 7) else None
  }

  /*
    Returns the total cost af any item.
    If that item has a price, then the price + 7% of the price should be returned.
  */
  def totalCost(item: Item): Option[Double] = {
    if (item.price.isDefined) Some(item.price.get * 1.07) else None
  }

  /*
    Given a list of weather temperatures, calculates the average temperature across all weather stations.
    Some weather stations don't report temperature
    Returns None if the list is empty or no weather stations contain any temperature reading.
   */
  def averageTemperature(temperatures: List[WeatherStation]): Option[Int] = {
    val temps: List[WeatherStation] = temperatures.filter(_.temperature.isDefined)
    if (temps.nonEmpty) Some(temps.foldLeft(0)((a, station) => a + station.temperature.get) / temps.length) else None
  }
}
