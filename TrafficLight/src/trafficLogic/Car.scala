package trafficLogic

import mapLogic._
import graphical._
import java.awt.geom.Point2D
import scala.math._

class Car(game: Game) {
  val length: Double = randomLength()
  val width: Double = widthForLength()
  val mass: Double = massForDimensions()

  def randomLength(): Double = {
    1.0 * Constants.laneWidth //Dummy value for now, will be random.
  }

  def widthForLength(): Double = {
    0.5 * Constants.laneWidth //Dummy value for now, will be random.
  }

  def massForDimensions(): Double = {
    1000 //Dummy value for now, will be random.
  }
  
  var location = new Point2D.Double(0,300) 
  def cornerLocation: Point2D.Double = {
    val controlPoint = new Point2D.Double(x-length/2, y-width/2)
    val helpLength = location distance controlPoint
    val dir = direction - acos((length/2)/helpLength)
    new Point2D.Double(x - helpLength * cos(dir), y - helpLength * sin(dir))
  }
  var direction: Double = 0
  private var speed: Double = Constants.maxSpeed // Units/seconds
  private def nanoSpeed = speed / 1E9  // Units/nanoSeconds
  private var acceleration: Double = 0 //Units/seconds^2
  private var accDirection: Double = 0
  
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
  /**Calculates new values based on the time passed since last update.
   * Cars ask whether the road in front of them is open and if no, where is the obstacle. Also the
   * cars only know the next road they are heading to - which is calculated always after exiting a
   * crossing or after being created - and they ask the current Road which lane they should use to achieve that.
   * @param time: Time since the last calculation in milliseconds
   */
  def calc(time: Long) = {
     
    location = new Point2D.Double(x + time*nanoSpeed, y)
  
  }
  
  
}