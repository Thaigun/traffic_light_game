package trafficLogic

import mapLogic._
import scala.math._
import graphical._
import java.awt.geom.Point2D
import mapLogic.Constants

 /*
 @param road: The road that this lane belongs to
 @param laneNumber: The number of this lane in corresponding road, counted from left.
 */
class Lane(val road: Road, val laneNumber: Int) {
  lazy val previousLane: Option[Lane] = if (road.previousRoad.isDefined) road.previousRoad.get.lanes.find(_.nextLane.get == this) else  None
  lazy val nextLane: Option[Lane] = findNextLane
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
  
  // Parameter d tells the one-dimensional coordinate on this lane, the distance from the startM towards the point on this lane
  def pointAtDistance(d: Double) = {
    new Point2D.Double(startM.getX() + cos(road.rotation) * d, startM.getY() + sin(road.rotation) * d)
    
  }
  
  private def findNextLane: Option[Lane] = {
    if (!road.hasNextRoad) return None
    if (road.nextRoad.get.leftIsTouching) {
      if (laneNumber < road.nextRoad.get.numOfLanes) road.nextRoad.get(laneNumber) else Some(road.nextRoad.get.right)
    } else {
      val thisRev = road.lanes.reverse
      val otherRev = road.nextRoad.get.lanes.reverse
      val i = road.numOfLanes - laneNumber - 1
      if (i < otherRev.size) Some(otherRev(i)) else Some(otherRev.last)
    }
  }
  
  override def toString = "Lane number: "+laneNumber+", road: "+road.id
}