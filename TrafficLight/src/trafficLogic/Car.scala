package trafficLogic

import mapLogic._
import graphical._
import java.awt.geom.AffineTransform
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import java.awt.geom.Area
import java.awt.Shape
import scala.math._
import mapLogic.Constants

class Car(game: Game, firstGoal: NavGoal) {
  class AccelVector(var acceleration: Double = 0, var direction: Double = 0) {
    def +(other: AccelVector) = {
      val x = cos(direction) * acceleration + cos(other.direction) * other.acceleration
      val y = sin(other.direction) * acceleration + sin(other.direction) * other.acceleration
      val dir = atan2(y, x)
      val force = sqrt(acceleration * acceleration + other.acceleration * other.acceleration)
      new AccelVector(force, dir)

    }
  }
  class SpeedVector(var velocity: Double = 0, var direction: Double = 0) {
    def offsetAfter(seconds: Double) = {
      val dx = cos(direction) * velocity * seconds
      val dy = sin(direction) * velocity * seconds
      new Point2D.Double(dx, dy)
    }
    /**
     * How should this SpeedVector be accelerated to achieve another desired speed and direction
     */
    def steerFor(other: SpeedVector, time: Double) = {
      val diff = other - this
      new AccelVector(diff.velocity * 2, diff.direction)
    }
    def unary_- = new SpeedVector(velocity, direction - Pi)

    def -(other: SpeedVector) = { -other + this }
    /** Normal addition of two speed vectors */
    def +(other: SpeedVector) = {
      val newdX = this.offsetAfter(1).getX() + other.offsetAfter(1).getX()
      val newdY = this.offsetAfter(1).getY() + other.offsetAfter(1).getY()
      val velo = sqrt(newdX * newdX + newdY * newdY)
      val dir = atan2(newdY, newdX)
      new SpeedVector(velo, dir)
    }
    /**
     * Finds and returns a SpeedVector that we get by accelerating this SpeedVector with a given AccelerationVector for a given time
     */
    def operate(acc: AccelVector, secs: Double): SpeedVector = {
      val dVx = acc.acceleration * secs * cos(acc.direction)
      val dVy = acc.acceleration * secs * sin(acc.direction)
      val targetDir = atan2(dVy, dVx)
      this + new SpeedVector(sqrt(dVx * dVx + dVy * dVy), targetDir)
    }
    override def toString() = "velocity: " + velocity + "\n, direction: " + direction
  }

  val length: Double = getLength()
  val width: Double = widthForLength()
  val break = breaking

  def breaking = -50

  def getLength(): Double = 1.0 * Constants.laneWidth

  def widthForLength(): Double = 0.5 * length

  def direction = speed.direction
  def velocity = speed.velocity
  def acceleration = steering.acceleration

  var location = new Point2D.Double(0, 0)
  //Location of the rear left corner
  def cornerLocation: Point2D.Double = {
    val controlPoint = new Point2D.Double(x - length / 2, y - width / 2)
    val helpLength = location distance controlPoint
    val dir = direction + acos((length / 2) / helpLength)
    new Point2D.Double(x - helpLength * cos(dir), y - helpLength * sin(dir))
  }
  //Location of the front left corner
  def frontLocation = {
    val xCoord = cornerLocation.getX + length * cos(direction)
    val yCoord = cornerLocation.getY + length * sin(direction)
    new Point2D.Double(xCoord, yCoord)
  }
  var speed = new SpeedVector
  private var steering = new AccelVector

  def cornerX = cornerLocation.getX()
  def cornerY = cornerLocation.getY()
  def x = location.getX()
  def y = location.getY()

  var nextCrossingLane: Option[CrossingLane] = None
  var currentCrossingLane: Option[CrossingLane] = None
  var currentLane: Option[Lane] = None
  var nextRoad: Option[Road] = None

  def hasCurrentLane = currentLane.isDefined
  def hasCurrentCross = currentCrossingLane.isDefined
  def hasNextRoad = nextRoad.isDefined
  // The point where the car is heading to next
  var navGoal = firstGoal
  def nextLeg: Point2D.Double = navGoal.position
  private def steeringType: Car.Steering = { if (navGoal.kind == NavGoal.redLights) Car.Arrive else Car.Seek }

  def hasArrived = steeringType == Car.Arrive && (location distance nextLeg) - length / 2 < Constants.preferredGap

  def goalCheck = {
    if (navGoal.kind == NavGoal.goal && passedPoint(nextLeg) && ((location distance nextLeg) < Constants.laneWidth)) {
      true
    } else { false }
  }

  /*
   * Space needed to stop from a given speed.
   */
  def stoppingDistance(forVelocity: Double = velocity) = 0.5 * forVelocity * forVelocity / -break

  lazy val graphic = new CarGraph(this)

  //Current
  def road: Option[Road] = Some(currentLane.getOrElse(return None).getRoad)

  def nextCrossing: Option[Crossing] = navGoal.onRoad.nextCrossing
  /*
   * Has the car gone past some point. This is used to tell when to set the next leg.
   */
  def passedPoint(pt: Point2D.Double) = {
    val compY = tan(direction + Pi / 2) * (pt.getX() - frontLocation.getX()) + frontLocation.getY()
    val passed = if (abs(direction % (2 * Pi)) < Pi / 2 || abs(direction % (2 * Pi)) > 3 * Pi / 2) pt.getY() < compY else pt.getY() > compY
    passed && this.location.distance(pt) < Constants.preferredGap + length / 2
  }
  /**
   * The target lane on the current road, that is, which lane the car should take to get to the next desired road.
   */
  def targetLane: Option[Lane] = {
    if (navGoal.kind == NavGoal.goal) return currentLane
    if (!hasNextRoad) return None
    if (!hasCurrentLane) return Some(nextRoad.get.right)
    if (hasCurrentLane) return road.get.whichLaneFor(currentLane.get, nextRoad.get)
    None
  }
  /**
   * The area in front of this car, that needs to be clear in order to drive with full speed. It is a rectangle a little wider than the car itself
   * starting from the front of it and continuing until the breaking distance.
   */
  protected def getScopeStart(wider: Double) = {
    new Point2D.Double(frontLocation.getX() + sin(direction) * wider, frontLocation.getY() - cos(direction) * wider)
  }
  /*
   * The scope that the car observes to avoid other cars.
   */
  def scope = {
    //How much the area is wider than the car
    val wider = width * 0.2
    val startCorner = getScopeStart(wider)
    val area = new Rectangle2D.Double(startCorner.getX(), startCorner.getY(), stoppingDistance(), width + 2 * wider)
    val trans = new AffineTransform {
      rotate(direction, startCorner.getX, startCorner.getY)
    }
    trans.createTransformedShape(area)
  }

  /**
   * Calculates new values based on the time passed since last update.
   * Cars ask whether the road in front of them is open and if no, where is the obstacle. Also the
   * cars only know the next road they are heading to - which is calculated always after exiting a
   * crossing or after being created - and they ask the current Road which lane they should use to achieve that.
   * @param time: Time since the last calculation in milliseconds
   */
  def calc(time: Long): Unit = {

    if (this.passedPoint(nextLeg) || this.navGoal.kind == NavGoal.greenLights || this.navGoal.kind == NavGoal.redLights) {
      findNextLeg
    }

    if (hasArrived) return

    val sec = time / 10E8
    val distanceToNext = (location distance nextLeg) - Constants.preferredGap - length / 2

    if (game.otherCarIn(scope, Some(this)).isDefined) {
      val space = game.spaceIn(this) + length / 2 // + length/2 because the steering takes in account the distance from the middle of the car
      if (space - length / 2 - Constants.preferredGap < 1) {
        return
      }
      val tempLegDir = Constants.angle(location, nextLeg)
      this.steering = arriveTowards(new Point2D.Double(location.getX() + cos(tempLegDir) * space, location.getY() + sin(tempLegDir) * space), sec)

    } else {
      if (steeringType == Car.Seek || distanceToNext > this.stoppingDistance()) {
        this.steering = steerTowards(nextLeg, sec)
      } else {
        if (hasArrived) return
        this.steering = arriveTowards(nextLeg, sec)
      }
    }

    speed = speed.operate(steering, sec)
    speed.velocity = min(Constants.maxSpeed, speed.velocity)
    val otherInScope = game.otherCarIn(this.graphic.outline, Some(this))
    if (otherInScope.isDefined) {
      if (otherInScope.get.location.getX() > location.getX())
        speed.velocity *= 0.9
    }
    val dx = cos(direction) * velocity * sec
    val dy = sin(direction) * velocity * sec
    location = new Point2D.Double(x + dx, y + dy)

  }

  /*
   * AccelVector for driving through a desired point
   */
  def steerTowards(pt: Point2D.Double, time: Double) = {
    //If the car is coming to - or is in - a crossing, the speed will be reduced 
    val desiredVelocity = if (this.currentCrossingLane.isDefined) {
      Constants.maxSpeed * (this.currentCrossingLane.get.length / max(this.currentCrossingLane.get.crossing.width, this.currentCrossingLane.get.crossing.height))
    } else { Constants.maxSpeed }

    //The car heads towards the middle of the lane (midPoint) if it's to far away from it.
    val desiredSpeed = vectorTowardsMiddle + new SpeedVector(desiredVelocity, Constants.angle(location, pt))
    speed.steerFor(desiredSpeed, time)

  }

  /*
   * AccelVector for arriving to the desired point
   */
  def arriveTowards(pt: Point2D.Double, time: Double) = {
    val distance = (pt distance location) - Constants.preferredGap - length / 2
    val speedForDistance = (10 / Constants.maxSpeed) * (sqrt(2 * (distance) * -break))
    val desiredVelocity = min(Constants.maxSpeed, speedForDistance)
    val desiredSpeed = vectorTowardsMiddle + new SpeedVector(desiredVelocity, Constants.angle(location, pt))
    speed.steerFor(desiredSpeed, time)
  }

  /*
   * SpeedVector that leads towards the middle of the current lane
   */
  private def vectorTowardsMiddle = {
    val midPoint = currentLane.get.pointAtDistance(location.distance(currentLane.get.startM))
    if ((location distance midPoint) > Constants.laneWidth * 0.15 && navGoal.kind != NavGoal.roadStart) {
      new SpeedVector(location.distance(midPoint), Constants.angle(location, midPoint))
    } else {
      new SpeedVector(0.0, road.get.rotation)
    }
  }

  def isInScope(area: Shape) = {
    val tryIntersection = new Area(area)
    tryIntersection.intersect(graphic.outline)
    !tryIntersection.isEmpty()
  }
  /**
   * Checks what is the one-dimensional location of this car in another cars scope, that is, distance from the start side of the other car's scope
   */
  def positionInScope(another: Car): Double = {
    val wider = another.width * 0.2
    val scopeStart = another.getScopeStart(wider)
    val maxLength = another.stoppingDistance() + 5
    var currentLength = 1.0
    val step = 1
    var area = new Rectangle2D.Double(scopeStart.getX(), scopeStart.getY(), currentLength, another.width + 2 * wider)
    val trans = new AffineTransform {
      rotate(direction, scopeStart.getX, scopeStart.getY)
    }
    var trySubScope = trans.createTransformedShape(area)
    while (!isInScope(trySubScope) && currentLength <= maxLength) {
      currentLength += step
      area = new Rectangle2D.Double(scopeStart.getX(), scopeStart.getY(), currentLength, another.width + 2 * wider)
      trySubScope = trans.createTransformedShape(area)
    }
    currentLength
  }

  override def toString = "Car no. " + (game.cars.indexOf(this) + 1) + " at " + location.getX() + ", " + location.getY()

  /**
   * This method finds the next leg for this car after it has passed the previous one.
   * The result depend on the previous goal's type (kind), next road's next road or crossing etc.
   */
  def findNextLeg(): Unit = {

    def handleGreen = {
      val crossLane = this.nextCrossingLane.getOrElse(throw new Exception("A car did not find the next CrossingLane"))
      if (crossLane.isEnabled && this.passedPoint(this.nextLeg)) {
        val newGoal = new NavGoal(crossLane.out.startM.getX(), crossLane.out.startM.getY(), this.nextRoad.get) {
          kind = NavGoal.roadStart
        }
        this.navGoal = newGoal
        this.currentCrossingLane = Some(crossLane)
      } else if (!crossLane.isEnabled && this.stoppingDistance() > this.location.distance(this.nextLeg)) {
        this.navGoal.kind = NavGoal.redLights
      }
    }

    def handleRed = {
      val crossLane = this.nextCrossingLane.getOrElse(throw new Exception("A car did not find the next CrossingLane, " + game.cars.indexOf(this)))
      if (crossLane.isEnabled) {
        this.navGoal.kind = NavGoal.greenLights
      }
    }

    def handleEnd = {
      if (this.passedPoint(this.nextLeg)) {
        val newGoal = new NavGoal(this.currentLane.get.nextLane.get.startM.getX(), this.currentLane.get.nextLane.get.startM.getY(), this.nextRoad.get) {
          kind = NavGoal.roadStart
        }
        this.navGoal = newGoal
      }
    }

    def handleStart: Unit = {
      if (!passedPoint(nextLeg)) return

      val newRoad = this.nextRoad.get
      val lane = if (this.hasCurrentCross) this.currentCrossingLane.get.out else this.currentLane.get.nextLane.get
      // A point a little before the lanes end, this makes the steering behavior more realistic and prevents from collisions in crossings
      val goalPoint = lane.navEndPoint
      val newGoal = NavGoal(goalPoint, newRoad)
      if (newRoad.endsToEdge) newGoal.kind = NavGoal.goal
      else if (newRoad.hasNextCross) newGoal.kind = NavGoal.redLights
      else if (newRoad.hasNextRoad) newGoal.kind = NavGoal.roadEnd

      this.navGoal = newGoal

      this.currentCrossingLane = None
      this.currentLane = Some(lane)
      this.nextRoad = this.road.get.raffleNextRoad

      if (newRoad.hasNextCross) {
        this.nextCrossingLane = this.nextCrossing.get.laneFromTo(this.currentLane.get, this.nextRoad.get)
        checkLaneSwitch
      } else {
        this.nextCrossingLane = None
      }
    }

    def handleGoal = {

    }

    def handleSwitch = {
      if (this.navGoal.kind == NavGoal.laneSwitch && this.passedPoint(this.nextLeg)) {
        this.navGoal.position = this.currentLane.get.navEndPoint
        if (this.road.get.hasNextRoad) this.navGoal.kind = NavGoal.roadEnd
        else if (this.road.get.hasNextCross) this.navGoal.kind = NavGoal.redLights
        else if (this.road.get.endsToEdge) this.navGoal.kind = NavGoal.goal

      }
    }

    this.navGoal.kind match {
      case NavGoal.greenLights => handleGreen
      case NavGoal.redLights => handleRed
      case NavGoal.roadEnd => handleEnd
      case NavGoal.roadStart => handleStart
      case NavGoal.goal => handleGoal
      case NavGoal.laneSwitch => handleSwitch
    }
  }

  def checkLaneSwitch = {
    if (this.hasCurrentLane && this.targetLane.isDefined) {
      if (this.currentLane.get != this.targetLane.get) {
        this.navGoal.kind = NavGoal.laneSwitch
        this.navGoal.position = targetLane.get.pointAtDistance(min(location.distance(currentLane.get.startM) + 2 * length, this.road.get.length))
        this.currentLane = this.targetLane
        if (this.nextCrossing.isDefined) nextCrossingLane = nextCrossing.get.laneFromTo(this.currentLane.get, this.nextRoad.get)
      }
    }
  }

}
object Car extends Enumeration {
  type Steering = Value
  val Seek, Arrive = Value
}