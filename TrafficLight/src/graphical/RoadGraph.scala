package graphical

import trafficLogic._
import java.awt.geom.Arc2D;
import java.awt.geom.AffineTransform
import java.awt.geom.Rectangle2D
import java.awt.Shape
import java.awt.geom.Line2D
import scala.swing._
import scala.math._

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
    //The starting angle of the sector to be drawn
    val startAng = if (road.rightIsTouching) {
      toDegrees(-Constants.angle(road.right.startR, road.left.startL))
    } else {
      toDegrees(-Constants.angle(road.left.startL, road.right.startR))
    }
    //The width of the sector
    val angExt = if (road.rightIsTouching) {
      toDegrees(-Constants.angle(road.previousRoad.get.right.endR, road.previousRoad.get.left.endL)) - startAng
    } else {
      toDegrees(-Constants.angle(road.previousRoad.get.left.endL, road.previousRoad.get.right.endR)) - startAng
    }
    sector.setArcByCenter(road.touchCorner.get.getX(), road.touchCorner.get.getY(), Constants.laneWidth * road.numOfLanes, startAng, angExt, 2)

    for (i <- sectorLines.indices) {
      sectorLines(i) = new Arc2D.Double() {
        setArcByCenter(road.touchCorner.get.getX(), road.touchCorner.get.getY(), Constants.laneWidth * (i + 1), startAng, angExt, 0)
      }
    }
  } else if (road.hasPrevCross) {
    //The starting angle of the sector to be drawn
    val startAng = if (road.rightIsTouching) {
      toDegrees(-Constants.angle(road.right.startR, road.left.startL))
    } else {
      toDegrees(-Constants.angle(road.left.startL, road.right.startR))
    }
    //The width of the sector
    val angExt = if (road.rightIsTouching) {
      toDegrees(-road.previousCrossing.get.getSide(road).get.angRightToLeft) - startAng
    } else {
      toDegrees(-road.previousCrossing.get.getSide(road).get.angLeftToRight) - startAng
    }
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
    //The starting angle of the sector to be drawn
    val startAng = if (!road.nextCrossing.get.rightEndIsTouching(road)) {
      toDegrees(-Constants.angle(road.right.startR, road.left.startL))
    } else {
      toDegrees(-Constants.angle(road.left.startL, road.right.startR))
    }
    //The width of the sector
    val angExt = if (!road.nextCrossing.get.rightEndIsTouching(road)) {
      toDegrees(-road.previousCrossing.get.getSide(road).get.angRightToLeft) - startAng
    } else {
      toDegrees(-road.nextCrossing.get.getSide(road).get.angLeftToRight) - startAng
    }
    sector.setArcByCenter(road.nextCrossing.get.getTouchingPointFor(road).getX(), road.nextCrossing.get.getTouchingPointFor(road).getY(), 
        Constants.laneWidth * road.numOfLanes, startAng, angExt, 2)

    for (i <- sectorLines.indices) {
      sectorLines(i) = new Arc2D.Double() {
        setArcByCenter(road.nextCrossing.get.getTouchingPointFor(road).getX(), road.nextCrossing.get.getTouchingPointFor(road).getY(), 
            Constants.laneWidth * (i + 1), startAng, angExt, 0)
      }
    }
  }
}