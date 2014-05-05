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
      if (whichRoad.hasNextCross) {
        nextCrossingLane = {
          whichRoad.nextCrossing.get.laneFromTo(lane, next.get)
        }
      }
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

  def over = score >= goal
  var panel: GamePanel = null
  var score = 0
  var goal = 10
  var resigned = false
  
  var started: Long = 0
  def elapsed = (System.currentTimeMillis() - started) / 1000.0

  def run() = {
    var started = System.currentTimeMillis()
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
  }

  def findNextLeg(car: Car): Unit = {
    // passedPoint(nextLeg) || hasArrived || navGoal.kind == NavGoal.redLights || navGoal.kind == NavGoal.greenLights
    //    println(car.passedPoint(car.nextLeg))
    //    println(car.navGoal.kind)
    //    println(car.navGoal.onRoad.id)
    //    if (car.navGoal.kind == NavGoal.greenLights) {
    //      val nextCrossing = car.navGoal.onRoad.nextCrossing.getOrElse(throw new Exception("NavGoal-type can't be red or green lights if there is no next crossing"))
    //      val crossLane = nextCrossing.lanes.find(lane => lane.in == car.currentLane.get && lane.out.getRoad == car.nextRoad.get).get
    //      if (!crossLane.isEnabled) { car.navGoal.kind = NavGoal.redLights }
    //      else if (car.passedPoint(car.nextLeg)) {
    //        car.navGoal = new NavGoal(crossLane.out.startM.getX(), crossLane.out.startM.getY(), crossLane.out.getRoad) {
    //          kind = NavGoal.roadStart
    //        }
    //        car.currentLane = Some(crossLane.out)
    //        
    //      }
    println
    println(car.navGoal.kind)
    println(car)
    println("nextCrossing: "+car.nextCrossingLane.isDefined)
    if (car.nextCrossing.isDefined)println("next crossing: " + car.nextCrossing.get.id)
    
    def handleGreen = {
      val crossLane = car.nextCrossingLane.getOrElse(throw new Exception("A car did not find the next CrossingLane"))
      if (crossLane.isEnabled && car.passedPoint(car.nextLeg)) {
        val newGoal = new NavGoal(crossLane.out.startM.getX(), crossLane.out.startM.getY(), car.nextRoad.get) {
          kind = NavGoal.roadStart
        }
        car.navGoal = newGoal
        car.currentCrossingLane = Some(crossLane)
      } else if (!crossLane.isEnabled) {
        car.navGoal.kind = NavGoal.redLights
      }
    }

    def handleRed = {
      println("In handleRed, next crossing lane id:")
      println(car.nextCrossingLane.get.id)
      val crossLane = car.nextCrossingLane.getOrElse(throw new Exception("A car did not find the next CrossingLane, "+cars.indexOf(car)))
      if (crossLane.isEnabled) {
        car.navGoal.kind = NavGoal.greenLights
      }
    }

    def handleEnd = {
      if (car.passedPoint(car.nextLeg)) {
        val newGoal = new NavGoal(car.currentLane.get.nextLane.get.startM.getX(), car.currentLane.get.nextLane.get.startM.getY(), car.nextRoad.get) {
          kind = NavGoal.roadStart
        }
        car.navGoal = newGoal
      }
    }

    def handleStart = {
      val road = car.nextRoad.get
      val lane = if (car.hasCurrentCross) car.currentCrossingLane.get.out else car.currentLane.get.nextLane.get
      val newGoal = NavGoal(lane.endM, road)
      if (road.endsToEdge) newGoal.kind = NavGoal.goal
      else if (road.hasNextCross) newGoal.kind = NavGoal.redLights
      else if (road.hasNextRoad) newGoal.kind = NavGoal.roadEnd
     
      car.navGoal = newGoal
      
      car.currentCrossingLane = None
      car.currentLane = Some(lane)
      car.nextRoad = car.road.get.raffleNextRoad
      
      if (road.hasNextCross) {
        car.nextCrossingLane = car.nextCrossing.get.laneFromTo(car.currentLane.get, car.nextRoad.get)
      } else {
        car.nextCrossingLane = None
      }
    }

    def handleGoal = {
      
    }

    def handleSwitch = {
      if (car.navGoal.kind == NavGoal.laneSwitch && car.passedPoint(car.nextLeg)) {
        car.navGoal.position = car.currentLane.get.endM
        car.navGoal.kind = NavGoal.roadEnd
      }
    }

    car.navGoal.kind match {
      case NavGoal.greenLights => handleGreen
      case NavGoal.redLights => handleRed
      case NavGoal.roadEnd => handleEnd
      case NavGoal.roadStart => handleStart
      case NavGoal.goal => handleGoal
      case NavGoal.laneSwitch => handleSwitch
    }
    //
    //    } else if (car.navGoal.kind == NavGoal.redLights) {
    //      val nextCrossing = car.navGoal.onRoad.nextCrossing.getOrElse(throw new Exception("NavGoal-type can't be red or green lights if there is no next crossing"))
    //      val crossLane = nextCrossing.lanes.find(lane => lane.in == car.currentLane.get && lane.out.getRoad == car.nextRoad.get).get
    //      if (crossLane.isEnabled) {
    //        println("enable lights")
    //        car.navGoal.kind = NavGoal.greenLights
    //      }
    //
    //    } else if (car.navGoal.kind == NavGoal.goal) {
    //      return // Score updating is handled by car-class
    //
    //    } else if (car.navGoal.kind == NavGoal.roadEnd) {
    //      try{
    //        car.currentLane = car.currentLane.get.nextLane
    //        car.nextRoad = car.road.get.raffleNextRoad
    //        car.navGoal = new NavGoal(car.currentLane.get.startM.getX(), car.currentLane.get.startM.getY(), car.road.get) {
    //          kind = NavGoal.roadStart
    //        }
    //      } catch{
    //        case a: Throwable => 
    //      }
    //    } else if (car.navGoal.kind == NavGoal.roadStart) {
    //      car.navGoal = NavGoal(car.currentLane.get.endM, car.road.get)
    //      if (car.road.get.endsToEdge) {
    //        car.navGoal.kind = NavGoal.goal
    //      } else if (car.road.get.hasNextRoad) {
    //        car.navGoal.kind = NavGoal.roadEnd
    //      } else if (car.road.get.hasNextCross) {
    //        car.navGoal.kind = NavGoal.redLights //If the light is green, it will be corrected during the next loop
    //      }
    //
    //    }
    //  }
  }
}
