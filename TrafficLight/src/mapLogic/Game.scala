package mapLogic

import trafficLogic._
//import java.io._
import scala.io._

class Game {
  
  /* We're using immutable Vector as variable instead of mutable fixed value, because it will remain the same once initialized
   * Roads are added based on the file.
   * The variable cars will be initialized based on the number of cars defined in the file.
   * Same applies to the crossings.
   */
  var roads: Array[Road] = Array() 
  var cars: Array[Car] = Array()
  var crossings: Array[Crossing] = Array()
  
  /*
   * The gameFile is not passed to the fileReader as a constructor param, because we might want to re-init the game with the same map. In
   * that case it is better to have the file saved in a different variable.
   */
  var gameFile: BufferedSource = null
  val fileReader = new FileReader(this)
  def readFile() {
    if (gameFile == null) throw new Exception("You must choose the game file first!")
    fileReader.setFile(gameFile)
    
    fileReader.read
    
    check
  }
  
  def setCars(n: Int) = {
    cars = Array.tabulate(n)(n => new Car(this))
  }
  
  def setRoads(roadArr: Array[Road]) = {
    this.roads = roadArr
  }
  
  def setCrossings(crossingArr: Array[Crossing]) = {
    this.crossings = crossingArr
  }
  
  //Checks that the map is correctly builded and no conflicts are encountered.
  def check = {}
}