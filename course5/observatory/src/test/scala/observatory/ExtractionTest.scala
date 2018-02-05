package observatory

import java.time.LocalDate

import org.apache.spark.sql.Dataset
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._
import utils.SparkJob

@RunWith(classOf[JUnitRunner])
trait ExtractionTest extends FunSuite with SparkJob {

  val year = 1975
  val debug = true

  val stationsPath:String = "/observatory/stations.csv"
  val temperaturePath:String = s"/observatory/$year.csv"

  lazy val stationsV1:Dataset[Station] = Extraction.stationsV1(stationsPath).persist
  lazy val temperaturesV1:Dataset[TemperatureRecord] = Extraction.temperaturesV1(year, temperaturePath).persist
  lazy val joinedV1:Dataset[StationDateLocationTemp] = Extraction.joinStationsWithTemperaturesV1(stationsV1, temperaturesV1).persist

  lazy val stationsV2:Dataset[Station] = Extraction.stationsV1(stationsPath).persist
  lazy val temperaturesV2:Dataset[TemperatureRecord] = Extraction.temperaturesV1(year, temperaturePath).persist
  lazy val joinedV2:Dataset[StationDateLocationTemp] = Extraction.joinStationsWithTemperaturesV1(stationsV1, temperaturesV1).persist


  lazy val locateTemperatures = Extraction.locateTemperatures(year, stationsPath, temperaturePath)
  lazy val locateAverage = Extraction.locationYearlyAverageRecords(locateTemperatures)

  test("stations"){
    if(debug) stationsV1.show()
    assert(stationsV1.filter((station:Station) => station.id=="007005").count()===0,"id: 007005")
    assert(stationsV1.filter((station:Station) => station.id=="007018").count()===0,"id: 007018")
    assert(stationsV1.filter((station:Station) => station.id=="725346~94866").count()===1,"id: 725346~94866")
    assert(stationsV1.filter((station:Station) => station.id=="725346").count()===1,"id: 725346")
    assert(stationsV1.filter((station:Station) => station.id=="~68601").count()===1,"id: ~68601")
    assert(stationsV1.count()===27708,"Num stations")
  }

  test("temperatures"){
    if(debug) temperaturesV1.show()
    assert(temperaturesV1.filter((tr:TemperatureRecord) => tr.id=="010010").count()===363,"id: 010010")
    assert(temperaturesV1.filter((tr:TemperatureRecord) => tr.id=="010010" && tr.day==1 && tr.month==1 && tr.temperature == (23.2-32)/9*5).count()===1,"id: 010010")
  }

  test("joined"){
    if(debug) joinedV1.show()
    assert(joinedV1.filter((jf:StationDateLocationTemp) => jf.date == StationDate(1,1,1975) && jf.location==Location(70.933,-008.667)).count()===1,"id: 010010 ")
    assert(joinedV1.filter((jf:StationDateLocationTemp) => jf.date == StationDate(1,1,1975) && jf.location==Location(70.933,-008.666)).count()===0,"no loc ")
  }

  test("locateTemperatures"){
    if(debug) locateTemperatures.take(20).foreach(println)
    assert(locateTemperatures.count(_._2==Location(70.933,-8.667)) === 363)
    assert(locateTemperatures.size === 2176493)
  }

  test("locationYearlyAverageRecords"){
    if(debug) locateAverage.take(20).foreach(println)
    assert(locateAverage.count(_._1==Location(70.933,-8.667)) === 1)
    assert(locateAverage.size === 8251)
  }



}