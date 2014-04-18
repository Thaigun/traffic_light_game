package trafficLogic

import mapLogic._
import graphical._

 /*
 @param road: The road that this lane belongs to
 @param laneNumber: The number of this lane in corresponding road, counted from left.
 */
class Lane(road: Road, laneNumber: Int) {
  
  val previousRoad: Option[Lane] = None
  val nextRoad: Option[Lane] = None
  val laneLeft: Option[Lane] = None
  val laneRight: Option[Lane] = None
  
  val graphic = new RoadGraph(startX, startY)(endX, endY)
}