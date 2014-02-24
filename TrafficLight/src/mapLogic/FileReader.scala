package mapLogic

import java.io._
import scala.io._
import trafficLogic._

class FileReader(game: Game) {
  var file: File = null
  var fileAsText = ""
  
  def setFile(newFile: File): Unit = {
    file = newFile
    setFile()
  }
  
  def setFile() = {
    if (file == null) throw new Exception("The game file must be defined first!")
  }
  
  def getNumberOfCars(): Int = {
    10 //Just a dummy value for now 
  }
  
  def getRoads(): Array[Road] = {
    Array(new Road(game))
  }
  
  def getCrossings(): Array[Crossing] = {
    Array(new Crossing)
  }
}