package mapLogic

import trafficLogic._
import graphical._
import javax.swing.Timer
import scala.io._
import scala.collection.mutable.Buffer
import java.awt.Shape
import java.awt.geom.Area
import scala.util.Random
import scala.math._

class Game extends Runnable {

  var panel: GamePanel = null // Will be initialized in readFile
  
  var roads: Array[Road] = Array()
  var cars: Buffer[Car] = Buffer()
  var crossings: Array[Crossing] = Array()
  var buttons: Array[CrossingButton] = Array()
  var size: (Int, Int) = (0, 0)

  /*
   * Initialize the file reader and read the game file.
   */
  var gameFile: BufferedSource = Source.fromFile("src/gamefile.txt")
  val fileReader = new FileReader(this)
  fileReader.setFile(gameFile)
  fileReader.scorefile = "src/highscore.txt"
  def readFile() {
    if (gameFile == null) throw new Exception("You must choose the game file first!")
    fileReader.read
    panel = new GamePanel(this)
    check
  }

  def setSize(measures: (Int, Int)) = { size = measures }

  def setRoads(roadArr: Array[Road]) = {
    this.roads = roadArr
  }

  def setCrossings(crossingArr: Array[Crossing]) = {
    this.crossings = crossingArr
  }

  def addThumbnails(thu: Array[CrossingButton]) = {
    this.buttons = thu
  }

  def createCar = {
    val edgeRoads = roads.filter(_.startsFromEdge)
    val random = new Random()
    val whichRoad = Constants.getRoadWeighted(edgeRoads)
    val lane = whichRoad.lanes(random.nextInt(whichRoad.numOfLanes))
    val next = whichRoad.raffleNextRoad

    val goal = new NavGoal(lane.endM.getX(), lane.endM.getY(), whichRoad)
    if (whichRoad.hasNextCross) goal.kind = NavGoal.redLights
    else if (whichRoad.endsToEdge) goal.kind = NavGoal.redLights
    else if (whichRoad.hasNextRoad) goal.kind = NavGoal.roadEnd

    val car = new Car(this, goal) {
      location = lane.startM
      speed = new SpeedVector(0, whichRoad.rotation)
      currentLane = Some(lane)
      navGoal = goal
      nextRoad = next

    }
    if (whichRoad.hasNextCross) {
      val nextcrossinglane = whichRoad.nextCrossing.get.laneFromTo(car.targetLane.get, next.get)
      car.nextCrossingLane = nextcrossinglane
    }
    
    car.navGoal.position = lane.navEndPoint

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

  def over = score >= goal
  var score = 0
  var goal = 10
  var carNumber = 10
  var resigned = false

  var started: Long = 0
  private var totalTime = 0.0
  def elapsed = {
    if (!resigned && !over) (System.currentTimeMillis() - started) / 1000.0 else totalTime
  }

  def run() = {
    buttons.foreach(_.listenTo(panel.mouse.clicks))
    cars.clear
    score = 0
    resigned = false

    started = System.currentTimeMillis()
    var delta: Long = 0
    var time: Long = 0
    var lastCarCreated = System.currentTimeMillis()

    def gameRound = {
      if (cars.size < carNumber && System.currentTimeMillis() - lastCarCreated > 1000) {
        createCar
        lastCarCreated = System.currentTimeMillis()
      } else if (System.currentTimeMillis() - lastCarCreated > 8000) {
        createCar
        lastCarCreated = System.currentTimeMillis()
      }

      cars.foreach(_.calc(delta))
      val inGoal = cars.filter(_.goalCheck)
      inGoal.foreach(cars -= _)
      score += inGoal.size

      panel.repaint
    }

    while (!over && !resigned) {
      time = System.nanoTime
      gameRound
      delta = System.nanoTime - time
    }
    totalTime = (System.currentTimeMillis() - started) / 1000.0
    panel.repaint
    panel.highScore
  }
}
