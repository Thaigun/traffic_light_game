package graphical

import trafficLogic._
import java.awt.geom.Point2D
import java.awt.geom.Arc2D
import java.awt.geom.AffineTransform
import java.awt.geom.Rectangle2D
import java.awt.Shape
import java.awt.geom.Line2D
import scala.swing._
import scala.math._
import mapLogic.Constants

class RoadGraph(val road: Road) {
  val rect = new Rectangle2D.Double(road.x1, road.y1, road.length, Constants.laneWidth * road.numOfLanes)

  val transform = new AffineTransform {
    rotate(road.rotation, road.x1, road.y1)
  }
  val outline = transform.createTransformedShape(rect)

  //Dotted lines between lanes.
  val trafLines: Array[Line2D] = Array.ofDim(road.numOfLanes - 1)
  for (i <- trafLines.indices) {
    val lane = road(i).get
    trafLines(i) = new Line2D.Double(lane.startR, lane.endR)
  }

  /*
   * The sector that fills the gap between this and the previous roadGraph
   * It's possible that the roads have a different number of lanes. The result is not the most beautiful but should work.
   */
  val sector = new Arc2D.Double()
  val sectorLines: Array[Arc2D] = Array.ofDim(road.numOfLanes - 1)

  if (road.hasPrevRoad) {
    //Starting and ending angles of the sector
    var startAng = 0.0
    var endAng = 0.0
    val prev = road.previousRoad.get
    if (road.rightIsTouching) {
      startAng = ang(road.right.startR, road.left.startL)
      endAng = ang(prev.right.startR, prev.left.startL)
    } else {
      startAng = ang(prev.left.endL, prev.right.endR)
      endAng = ang(road.left.startL, road.right.startR)
    }
    if (startAng < endAng) startAng += 2*Pi
    startAng = toDegrees(-startAng)
    endAng = toDegrees(-endAng)
    val angExt = endAng - startAng
    
    sector.setArcByCenter(road.touchCorner.get.getX(), road.touchCorner.get.getY(), Constants.laneWidth * road.numOfLanes, startAng, angExt, 2)

    for (i <- sectorLines.indices) {
      sectorLines(i) = new Arc2D.Double() {
        setArcByCenter(road.touchCorner.get.getX(), road.touchCorner.get.getY(), Constants.laneWidth * (i + 1), startAng, angExt, 0)
      }
    }
  } else if (road.hasPrevCross) {
    var startAng = 0.0
    var endAng = 0.0
    val prev = road.previousCrossing.get
    if (road.rightIsTouching) {
      startAng = ang(road.right.startR, road.left.startL)
      endAng = prev.getSide(road).get.angRightToLeft
    } else {
      startAng = prev.getSide(road).get.angLeftToRight
      endAng = ang(road.left.startL, road.right.startR)
    }
    if (startAng < endAng) startAng += 2*Pi
    startAng = toDegrees(-startAng)
    endAng = toDegrees(-endAng)
    val angExt = endAng - startAng   
    
    sector.setArcByCenter(road.touchCorner.get.getX(), road.touchCorner.get.getY(), Constants.laneWidth * road.numOfLanes, startAng, angExt, 2)

    for (i <- sectorLines.indices) {
      sectorLines(i) = new Arc2D.Double() {
        setArcByCenter(road.touchCorner.get.getX(), road.touchCorner.get.getY(), Constants.laneWidth * (i + 1), startAng, angExt, 0)
      }
    }
  }

  val endSect = new Arc2D.Double()
  val endSectLines: Array[Arc2D] = Array.ofDim(road.numOfLanes - 1)
  if (road.hasNextCross) {
    var startAng = 0.0
    var endAng = 0.0
    val next = road.nextCrossing.get
    if (next.rightEndIsTouching(road)) {
      startAng = next.getSide(road).get.angLeftToRight
      endAng = ang(road.right.endR, road.left.endL)
    } else {
      startAng = ang(road.left.endL, road.right.endR)
      endAng = next.getSide(road).get.angRightToLeft
    }
    if (startAng < endAng) startAng += 2*Pi
    startAng = toDegrees(-startAng)
    endAng = toDegrees(-endAng)
    val angExt = endAng - startAng
   
    endSect.setArcByCenter(next.getTouchingEndPointFor(road).getX(), next.getTouchingEndPointFor(road).getY(), 
        Constants.laneWidth * road.numOfLanes, startAng, angExt, 2)

    for (i <- endSectLines.indices) {
      endSectLines(i) = new Arc2D.Double() {
        setArcByCenter(next.getTouchingEndPointFor(road).getX(), next.getTouchingEndPointFor(road).getY(), 
            Constants.laneWidth * (i + 1), startAng, angExt, 0)
      }
    }
  }
  def ang(p1: Point2D.Double, p2: Point2D.Double) = Constants.angle(p1, p2)
}