package observatory

import java.time.LocalDate

import observatory.utils.{Resources, SparkJob}
import utils.Resources._
import org.apache.spark.sql.{Column, DataFrame, Dataset}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
/**
  * 1st milestone: data extraction
  */
object Extraction extends SparkJob {

  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    Grading.version match {
      case 1 => locateTemperaturesV1Dataset(year, stationsFile, temperaturesFile)
      case 2 => locateTemperaturesV2Dataframe(year, stationsFile, temperaturesFile)
      case _ => locateTemperaturesV1Dataset(year, stationsFile, temperaturesFile)
    }
  }

  /**
    * we want to compute the average temperature, over a year, for every station.
    *
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {

    Grading.version match {
      case 1 => locationYearlyAverageRecordsV1(records)
      case 2 => locationYearlyAverageRecordsV2(records)
      case _ => locationYearlyAverageRecordsV1(records)
    }
  }




  /*
    ####
    VERSION 1 - uses datasets
    courtesy to https://github.com/TomLous/coursera-scala-capstone
    ###
   */

  def stationsV1(stationsFile: String): Dataset[Station] = {
    spark
      .read
      .csv(resourcePath(stationsFile))
      .select(
        concat_ws("~", coalesce('_c0, lit("")), '_c1).alias("id"),
        '_c2.alias("latitude").cast(DoubleType),
        '_c3.alias("longitude").cast(DoubleType)
      )
      .where('_c2.isNotNull && '_c3.isNotNull && '_c2 =!= 0.0 && '_c3 =!= 0.0)
      .as[Station]
  }

  def temperaturesV1(year: Int, temperaturesFile: String): Dataset[TemperatureRecord] = {
    spark
      .read
      .csv(resourcePath(temperaturesFile) )
      .select(
        concat_ws("~", coalesce('_c0, lit("")), '_c1).alias("id"),
        '_c3.alias("day").cast(IntegerType),
        '_c2.alias("month").cast(IntegerType),
        lit(year).as("year"),
        (('_c4 - 32) / 9 * 5).alias("temperature").cast(DoubleType)
      )
      .where('_c4.between(-200, 200))
      .as[TemperatureRecord]
  }

  def joinStationsWithTemperaturesV1(stations: Dataset[Station], temperatures: Dataset[TemperatureRecord]):Dataset[StationDateLocationTemp] = {
    stations
      .join(temperatures, usingColumn = "id")
      .as[LocationDateTemp]
      .map(j => (StationDate(j.day, j.month, j.year), Location(j.latitude, j.longitude), j.temperature))
      .toDF("date", "location", "temperature")
      .as[StationDateLocationTemp]
  }


  /**
    * This method should return the list of all the temperature records converted in degrees Celsius
    * along with their date and location
    * (ignore data coming from stations that have no GPS coordinates).
    *
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperaturesV1Dataset(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationTemp = joinStationsWithTemperaturesV1(stationsV1(stationsFile), temperaturesV1(year, temperaturesFile))

    //    // It'stations a shame we have to use the LocalDate because Spark cannot encode that. hence this ugly bit
    stationTemp.collect()
      .par
      .map(
        station => (station.date.toLocalDate, station.location, station.temperature)
      ).seq
  }

  /**
    * we want to compute the average temperature, over a year, for every station.
    *
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecordsV1(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .par
      .groupBy(_._2)
      .mapValues(
        l => l.foldLeft(0.0)(
          (t,r) => t + r._3) / l.size
      )
      .seq
  }


  /*
    ####
    VERSION 2
    courtesy to https://github.com/honeyAndSw/coursera-scala/blob/master/course5/observatory
    ###
   */

  def stationsV2(stationsFile: String): DataFrame = {
    val stations = sparkJob.readCsv(Resources.resourcePath(stationsFile), stationsSchema)
      .where('latitude.isNotNull && 'longitude.isNotNull && 'latitude =!= 0.0 && 'longitude =!= 0.0)
    stations
  }

  def temperaturesV2(year: Int, temperaturesFile: String): DataFrame = {
    val temps = sparkJob.readCsv(Resources.resourcePath(temperaturesFile), temperaturesSchema)
      .where('temp.between(-200, 200))
    temps
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperaturesV2Dataframe(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = stationsV2(stationsFile)
    val temps = temperaturesV2(year, temperaturesFile)

    //      equalCond("stn")(stations, temps) && equalCond("wban")(stations, temps))
//    val joinOnStationId = temps.join(stations,
    val joinOnStationId = stations.join(temps,
      (
        (stations("stn").isNull && temps("stn").isNull) || (stations("stn") === temps("stn"))
      )
        ||
      (
        (stations("wban").isNull && temps("wban").isNull) || (stations("wban") === temps("wban"))
      )
    )
      .filter(stations("latitude").isNotNull && stations("longitude").isNotNull)

    val toTuples: Array[(LocalDate, Location, Double)] = joinOnStationId
//      .collect()
//      .par
      .map { row =>
        val date = LocalDate.of(year, row.getAs[Int]("month"), row.getAs[Int]("day"))
        val location = Location(row.getAs[Double]("latitude"), row.getAs[Double]("longitude"))
        (date, location, toCelsius(row.getAs[Double]("temp")))
      }
      .collect()

    toTuples
  }

  private def equalCond(col: String): (DataFrame, DataFrame) => Column = (d1, d2) => {
    (d1(col).isNull && d2(col).isNull) || (d1(col) === d2(col))
  }

  private def toCelsius(f: Double): Double = ((f + 40) / 1.8) - 40



  private val stationsSchema: StructType = StructType(Seq(
    StructField("stn", StringType),
    StructField("wban", StringType),
    StructField("latitude", DoubleType),
    StructField("longitude", DoubleType)
  ))

  private val temperaturesSchema: StructType = StructType(Seq(
    StructField("stn", StringType),
    StructField("wban", StringType),
    StructField("month", IntegerType),
    StructField("day", IntegerType),
    StructField("temp", DoubleType)
  ))

  /**
    * we want to compute the average temperature, over a year, for every station.
    *
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecordsV2(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .par
      .foldLeft(Map[Location, (Double, Int)]()) { case (map, (_, loc, temp)) =>
        if (map.contains(loc)) {
          val prev = map(loc)
          map + ((loc, (prev._1 + temp, prev._2 + 1)))
        } else {
          map + ((loc, (temp, 1)))
        }
      }
      .map { case (loc, (temp, size)) => (loc, temp / size.toDouble) }
  }



}
