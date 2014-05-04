package mapLogic

import trafficLogic._
import graphical._
import javax.swing.Timer
import scala.io._
import scala.collection.mutable.Buffer
import java.awt.Shape

class Game extends Runnable {

  /* We're using immutable Vector as variable instead of mutable fixed value, because it will remain the same once initialized
   * Roads are added based on the file.
   * The variable cars will be initialized based on the number of cars defined in the file.
   * Same applies to the crossings.
   */
  var roads: Array[Road] = Array()
  var cars: Buffer[Car] = Buffer(new Car(this))
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

  def setRoads(roadArr: Array[Road]) = {
    this.roads = roadArr
  }

  def setCrossings(crossingArr: Array[Crossing]) = {
    this.crossings = crossingArr
  }
  
  def createCar = {
    cars += new Car(this)
  }
  
  def allSpaceIn(area: Shape): Boolean = {
    true
  }
  
  def spaceIn(area: Shape): Double = {
    900
  }

  //Checks that the map is correctly builded and no conflicts are encountered.
  def check = {}
  
  def over = score >= Constants.goal
  var panel: GamePanel = null
  var score = 0

  def run() = {
    var delta: Long = 0
    var time: Long = 0
    var lastCarCreated = System.currentTimeMillis()
    //Remove:
    createCar

    def gameRound = {
//      if (cars.size < Constants.carNumber && System.currentTimeMillis() - lastCarCreated > 1000) {
//        createCar
//      } else if (System.currentTimeMillis() - lastCarCreated > 20000) { createCar }
      
      
      
      cars.foreach(_.calc(delta))
      
      panel.repaint
    }

    while (!over) {
      time = System.nanoTime
      gameRound
      delta = System.nanoTime - time
    }
  }

}