package trafficLogic

import mapLogic._
import graphical._
import java.awt.geom.AffineTransform
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import scala.math._

class Car(game: Game) {
  class AccelVector(var acceleration: Double = 0, var direction: Double = 0) {
  }
  class SpeedVector(var velocity: Double = 0, var direction: Double = 0) {
    def offsetAfter(seconds: Double) = {
      val dx = cos(direction) * velocity * seconds
      val dy = sin(direction) * velocity * seconds
      new Point2D.Double(dx, dy)
    }
    def steerFor(other: SpeedVector) = {
      val thisOffset = this.offsetAfter(1)
      val otherOffset = other.offsetAfter(1)
      val amount = thisOffset distance otherOffset
      val direction = atan2(thisOffset.getX()-otherOffset.getX(), thisOffset.getY()-otherOffset.getY())
      new AccelVector(amount, direction)
    }
    def +(other: SpeedVector) ={
      val newdX = this.offsetAfter(1).getX() + other.offsetAfter(1).getX()
      val newdY = this.offsetAfter(1).getY() + other.offsetAfter(1).getY()
      val velo = sqrt(pow(newdX, 2) + pow(newdY, 2))
      val dir = atan2(newdY, newdX)
      new SpeedVector(velo, dir)
    }
    def operate(acc: AccelVector, secs: Double) = {
      val dVx = acc.acceleration * secs * cos(acc.direction)
      val dVy = acc.acceleration * secs * sin(acc.direction)
      val plusDirection = atan2(dVy, dVx)
      this + new SpeedVector(sqrt(dVx*dVx+dVy*dVy), plusDirection)
      
    }
  }
  
  val length: Double = randomLength()
  val width: Double = widthForLength()
  val mass: Double = massForDimensions()
  val maxAcceleration: Double = randomAcceleration
  val break = randomBreak
  
  def randomBreak = {
    -35
  }

  def randomAcceleration = {
    25
  }
  def randomLength(): Double = {
    1.0 * Constants.laneWidth //Dummy value for now, will be random.
  }

  def widthForLength(): Double = {
    0.5 * Constants.laneWidth //Dummy value for now, will be random.
  }

  def massForDimensions(): Double = {
    1000 //Dummy value for now, will be random.
  }
  
  def direction = speed.direction
  def velocity = speed.velocity
  def acceleration = steering.acceleration
  
  var location = new Point2D.Double(300,300) 
  //Location of the rear left corner
  def cornerLocation: Point2D.Double = {
    val controlPoint = new Point2D.Double(x-length/2, y-width/2)
    val helpLength = location distance controlPoint
    val dir = direction - acos((length/2)/helpLength)
    new Point2D.Double(x - helpLength * cos(dir), y - helpLength * sin(dir))
  }
  //Location of the front left corner
  def frontLocation = {
    val xCoord = cornerLocation.getX + length*cos(direction)
    val yCoord = cornerLocation.getY + length*sin(direction)
    new Point2D.Double(xCoord, yCoord)
  }
  private var speed = new SpeedVector(0, 0)
  private var steering = new AccelVector
  
  def cornerX = cornerLocation.getX()
  def cornerY = cornerLocation.getY()
  def x = location.getX()
  def y = location.getY()
  
  private var currentCrossingLane: Option[CrossingLane] = None
  private var currentLane: Option[Lane] = None
  private var nextRoad: Option[Road] = None
  
  def hasCurrentLane = currentLane.isDefined
  def hasCurrentCross = currentCrossingLane.isDefined
  def hasNextRoad = nextRoad.isDefined
 // The point where the car is heading to next
  private var nextLeg: Point2D.Double = new Point2D.Double(1800, 1000)
  private var steeringType: Car.Steering = Car.Seek
  def setNextLeg(pt: Point2D.Double, t: Car.Steering) = { nextLeg = pt; steeringType = t }
  
  def stoppingDistance(forVelocity: Double = velocity) = 0.5 * velocity * velocity / -break
  
  lazy val graphic = new CarGraph(this)
  
  //Current
  def road: Option[Road] = Some(currentLane.getOrElse(return None).getRoad)
  
  /**
   * The target lane on the current road, that is, which lane the car should take to get to the next desired road.
   */
  def targetLane: Option[Lane] = {
    if (!hasNextRoad) return None
    if (!hasCurrentLane && !hasCurrentCross) return Some(nextRoad.get.right)
    if (hasCurrentLane) return road.get.whichLaneFor(currentLane.get, nextRoad.get)
    if (hasCurrentCross) return Some(currentCrossingLane.get.out)
    None    
  } 
  /**
   * The area in front of this car, that needs to be clear in order to drive with full speed. It is a rectangle a little wider than the car itself
   * starting from the front of it and continuing until the breaking distance.
   */
  def scope = {
    //How much the area is wider than the car
    val wider = width * 0.2
    val startCorner = new Point2D.Double(frontLocation.getX() + sin(direction)*wider, frontLocation.getY() - cos(direction)*wider)
    val area = new Rectangle2D.Double(startCorner.getX(), startCorner.getY(), stoppingDistance(), width+2*wider)
    val trans = new AffineTransform {
      rotate(direction, startCorner.getX, startCorner.getY)
    }
    trans.createTransformedShape(area)
  }
  
  /**Calculates new values based on the time passed since last update.
   * Cars ask whether the road in front of them is open and if no, where is the obstacle. Also the
   * cars only know the next road they are heading to - which is calculated always after exiting a
   * crossing or after being created - and they ask the current Road which lane they should use to achieve that.
   * @param time: Time since the last calculation in milliseconds
   */
  def calc(time: Long) = {
    val sec = time / 10E8
    
    if (steeringType == Car.Seek) {
      this.steering = steerTowards(nextLeg, sec)
    } else {
      this.steering = arriveTowards(nextLeg, sec)
    }
    
    if (!game.allSpaceIn(scope)) {
      val space = game.spaceIn(scope) - Constants.preferredGap
      if (space > 0) steering.acceleration = -0.5 * velocity * velocity / space
    }
    
    speed = speed.operate(steering, sec)
    val dx = cos(direction) * velocity * sec
    val dy = sin(direction) * velocity * sec
    location = new Point2D.Double(x + dx, y + dy)
  }
  
  def steerTowards(pt: Point2D.Double, time: Double) = {
    val desiredVelocity = velocity + time * maxAcceleration
    val desiredSpeed = new SpeedVector(desiredVelocity, Constants.angle(location, pt))
    val acceleration = desiredSpeed steerFor speed
    acceleration
  }
  
  def arriveTowards(pt:Point2D.Double, time: Double): AccelVector = {
    val desiredVelocity = min(velocity + time * maxAcceleration, Constants.maxSpeed * ((pt distance location)*0.9) / this.stoppingDistance(Constants.maxSpeed))
    val desiredSpeed = new SpeedVector(desiredVelocity, Constants.angle(location, pt))
    val acceleration = desiredSpeed steerFor speed
    acceleration
  }
  
  
}

object Car extends Enumeration {
  type Steering = Value
  val Seek, Arrive = Value
}