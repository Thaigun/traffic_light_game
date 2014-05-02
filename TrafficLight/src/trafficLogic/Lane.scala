package trafficLogic

import mapLogic._
import scala.math._
import graphical._
import java.awt.geom.Point2D

 /*
 @param road: The road that this lane belongs to
 @param laneNumber: The number of this lane in corresponding road, counted from left.
 */
class Lane(road: Road, val laneNumber: Int) {
  lazy val previousLane: Option[Lane] = if (road.previousRoad.isDefined) road.previousRoad.get(laneNumber) else None
  lazy val nextLane: Option[Lane] = if (road.nextRoad.isDefined) road.nextRoad.get(laneNumber) else None
  lazy val laneLeft: Option[Lane] = road(laneNumber - 1)
  lazy val laneRight: Option[Lane] = road(laneNumber + 1)
  
  def startR = new Point2D.Double(road.x1 + xOffset(laneNumber+1), road.y1 + yOffset(laneNumber+1))
  def startM = new Point2D.Double((startL.x+startR.x)/2, (startL.y+startR.y)/2)
  def startL = new Point2D.Double(road.x1 + xOffset(laneNumber), road.y1 + yOffset(laneNumber))
  def endR = new Point2D.Double(road.end.getX() + xOffset(laneNumber+1), road.end.getY() + yOffset(laneNumber+1))
  def endM = new Point2D.Double((endL.x+endR.x)/2, (endL.y+endR.y)/2)
  def endL = new Point2D.Double(road.end.getX() + xOffset(laneNumber), road.end.getY() + yOffset(laneNumber))
  
  def getRoad = road
  def isLeft = road.left == this
  def isRight = road.right == this
  
  // Can be used to count an offset of some point for an amount of "road-widths" (n) given. Depends on roadWidth and the angle of the parent road. 
  def xOffset(n: Int) = -n * sin(road.rotation) * Constants.laneWidth
  def yOffset(n: Int) = n * cos(road.rotation) * Constants.laneWidth
  
  
  def spaceFor(car: Car) = Double.PositiveInfinity
}