package mapLogic

import java.io._
import java.awt.geom.Point2D
import scala.collection.mutable.Buffer
import scala.io._
import trafficLogic._
import scala.Predef._

/*
 * Roads: ROA0001, 2, 260.500, 900.500, -, 0002
 * Id, Number of lanes, starting coords, end coords, previous road, next road
 */
class FileReader(game: Game) {
  var file: BufferedSource = null
  val rowsInFile: Buffer[String] = Buffer()

  def setFile(newFile: BufferedSource): Unit = {
    file = newFile
    setFile()
  }

  def setFile() = {
    if (file == null) throw new Exception("The game file must be defined first!")
    val fileReader = file.getLines
    while (fileReader.hasNext) {
      val nextLine = fileReader.next.replaceAllLiterally(" ", "").takeWhile(_ != '#')
      if (!nextLine.isEmpty()) rowsInFile += nextLine
    }

  }

  def getNumberOfCars(): Int = {
    10 //Just a dummy value for now 
  }

  def getRoads(): Array[Road] = {
    val roadStrings = rowsInFile.filter(_.toLowerCase.startsWith("roa"))
    val roads: Array[Road] = Array.ofDim(roadStrings.size)
    // All roads are added to the 'roads' array
    for (i <- roads.indices) {
      val splitted = roadStrings(i).drop(3).split(',')
      require(splitted.size == 6, "The " + i + ". row in the gamefile is invalid (should have 6 fields)")
      val coordinates = Vector[Int](splitted(2).split('.')(0).toInt, splitted(2).split('.')(1).toInt, splitted(3).split('.')(0).toInt, splitted(3).split('.')(1).toInt)
      roads(i) = new Road(game, splitted(0), splitted(1).toInt)(coordinates(0), coordinates(1))(coordinates(2), coordinates(3))
    }
    /* Previous and next roads are assigned. This could not have been done in the previous part, because not all roads exist when they are needed for other roads.
     * First we find the corresponding string and then find and assign the correct "neighboring" roads.
     */
    for (road <- roads) {
      val stringOfRoad = roadStrings.find((s: String) => s.drop(3).startsWith(road.id))
      val prevId = stringOfRoad.getOrElse(throw new Exception("An error occured while reading the game file.")).split(',')(4)
      val nextId = stringOfRoad.getOrElse(throw new Exception("An error occured while reading the game file.")).split(',')(5)
      if (prevId.length() == 4) {
        road.previousRoad = roads.find(_.id == prevId)
      } else if (prevId.length() == 2) {
        road.previousRoad
      }

      if (nextId != "-") road.nextRoad = roads.find(_.id == nextId)
    }

    roads
  }

  def getCrossings(): Array[Crossing] = {
    val crossStrings = rowsInFile.filter(_.toLowerCase.startsWith("cro"))
    val crossings: Array[Crossing] = Array.ofDim(crossStrings.size)

    for (i <- crossings.indices) {
      val splitted = crossStrings(i).drop(3).split(',')
      val location = new Point2D.Double(splitted(1).split('.')(0).toDouble, splitted(1).split('.')(1).toDouble)
      crossings(i) = new Crossing(splitted(0), location, splitted(2).toCharArray())
    }

    crossings
  }
}