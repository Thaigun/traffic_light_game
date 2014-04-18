package mapLogic

import java.io._
import scala.collection.mutable.Buffer
import scala.io._
import trafficLogic._
import scala.Predef._

/*
 * Roads: ROA0001, 2, 260.500, 900.500, -, 0002
 * Id, Number of lanes, starting coords, end coords, previous road, next road
 */
class FileReader(game: Game) {
  var file: File = null
  val rowsInFile: Buffer[String] = Buffer()
  
  def setFile(newFile: File): Unit = {
    file = newFile
    setFile()
  }
  
  def setFile() = {
    if (file == null) throw new Exception("The game file must be defined first!")
    val fileReader = Source.fromFile(file).getLines
    while(fileReader.hasNext) {
      val nextLine = fileReader.next.trim().takeWhile(_ != '#')
      if (!nextLine.isEmpty()) rowsInFile += nextLine
    }
    
  }
  
  def getNumberOfCars(): Int = {
    10 //Just a dummy value for now 
  }
  
  def getRoads(): Array[Lane] = {
    val roadStrings = rowsInFile.filter(_.startsWith("LAN"))
    val roads: Array[Lane] = Array.ofDim(roadStrings.size)
    for (i <- roads.indices) {
      val splitted = roadStrings(i).drop(3).trim.split(',')
      require(splitted.size == 6, "The "+ i +". row in the gamefile is invalid (should have 6 fields)")
    }
    roads
  }
  
  def getCrossings(): Array[Crossing] = {
    Array(new Crossing)
  }
}