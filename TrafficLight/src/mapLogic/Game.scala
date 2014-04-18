package mapLogic

import trafficLogic._
import java.io._
import scala.io._

class Game {
  
  /* We're using immutable Vector as variable instead of mutable fixed value, because it will remain the same once initialized
   * Roads are added based on the file.
   * The variable cars will be initialized based on the number of cars defined in the file.
   * Same applies to the crossings.
   */
  var roads: Array[Lane] = Array() 
  var cars: Array[Car] = Array()
  var crossings: Array[Crossing] = Array()
  
  /*
   * The gameFile is not passed to the fileReader as a constructor param, because we might want to re-init the game with the same map. In
   * that case it is better to have the file saved in a different variable.
   */
  var gameFile: File = null
  val fileReader = new FileReader(this)
  def readFile() {
//    if (gameFile == null) throw new Exception("You must choose the game file first!")
//    fileReader.setFile(gameFile)
    
    setCars(fileReader.getNumberOfCars)
    setRoads(fileReader.getRoads)
    setCrossings(fileReader.getCrossings)
  }
  
  def setCars(n: Int) = {
    cars = Array.tabulate(n)(n => new Car(this))
  }
  
  def setRoads(roadArr: Array[Lane]) = {
    this.roads = roadArr
  }
  
  def setCrossings(crossingArr: Array[Crossing]) = {
    this.crossings = crossingArr
  }
  
  
}