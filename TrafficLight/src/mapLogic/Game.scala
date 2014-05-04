package mapLogic

import trafficLogic._
import graphical._
import javax.swing.Timer
import scala.io._
import scala.collection.mutable.Buffer
import java.awt.Shape
import java.awt.geom.Area
import scala.util.Random

class Game extends Runnable {

  /* We're using immutable Vector as variable instead of mutable fixed value, because it will remain the same once initialized
   * Roads are added based on the file.
   * The variable cars will be initialized based on the number of cars defined in the file.
   * Same applies to the crossings.
   */
  var roads: Array[Road] = Array()
  var cars: Buffer[Car] = Buffer()
  var crossings: Array[Crossing] = Array()
  var size: (Int, Int) = (0, 0)

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

  def setSize(measures: (Int, Int)) = { size = measures }

  def setRoads(roadArr: Array[Road]) = {
    this.roads = roadArr
  }

  def setCrossings(crossingArr: Array[Crossing]) = {
    this.crossings = crossingArr
  }

  def createCar = {
    val edges = roads.filter(_.startsFromEdge)
    val random = new Random()
    val whereTo = edges(random.nextInt(edges.size))
    val lane = whereTo.lanes(random.nextInt(whereTo.numOfLanes))
    val car = new Car(this) {
      location = lane.startM
      speed = new SpeedVector(0, whereTo.rotation)
      currentLane = Some(lane)
    }
    cars += car

  }
  /**
   * If there are no other cars within the scope, returns none, if there is, returns it in Some.
   */
  def otherCarIn(area: Shape, ignore: Option[Car] = None): Option[Car] = {
    val takenIntoAccount = if (ignore.isDefined) cars - ignore.get else cars
    for (car <- takenIntoAccount) {
      if (car.isInScope(area)) return Some(car)
    }
    None
  }

  def spaceIn(scopeOf: Car): Double = {
    val scope = scopeOf.scope
    val blocking = cars.filter(car => car.isInScope(scope)) - scopeOf
    var closest = scope.getBounds2D().getWidth() // This is just a starting value, that is meant to be bigger than any real.
    for (a <- blocking) {
      val positionInScope = a.positionInScope(scopeOf)
      if (positionInScope < closest) closest = positionInScope
    }
    closest
  }

  //Checks that the map is correctly builded and no conflicts are encountered.
  def check = {}

  def over = score >= Constants.goal
  var panel: GamePanel = null
  var score = 0
  var resigned = false

  def run() = {
    var delta: Long = 0
    var time: Long = 0
    var lastCarCreated = System.currentTimeMillis()

    def gameRound = {
      if (cars.size < Constants.carNumber && System.currentTimeMillis() - lastCarCreated > 1000) {
        createCar
        lastCarCreated = System.currentTimeMillis()
      } else if (System.currentTimeMillis() - lastCarCreated > 20000) { 
        createCar
        lastCarCreated = System.currentTimeMillis()  
      }

      cars.foreach(_.calc(delta))

      panel.repaint
    }

    while (!over && !resigned) {
      time = System.nanoTime
      gameRound
      delta = System.nanoTime - time
    }
  }
  
  def findNextLeg(car: Car) = {
    
  }

}