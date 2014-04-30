package graphical

import trafficLogic._
import java.awt.Polygon
import java.awt.geom.AffineTransform
import java.awt.geom.Point2D
import java.awt.geom.Line2D
import scala.math._

class CrossingLaneGraph(lane: CrossingLane) {
  /**
   * The following code has been almost entirely copied from http://stackoverflow.com/a/5446247/3280244 1.5.2014
   * Author: Michael Zlatkovsky
   */
  
  def fromPt = lane.start
  def toPt = lane.end

  val arrowPolygon = new Polygon()
  arrowPolygon.addPoint(-6, 1)
  arrowPolygon.addPoint(3, 1)
  arrowPolygon.addPoint(3, 3)
  arrowPolygon.addPoint(6, 0)
  arrowPolygon.addPoint(3, -3)
  arrowPolygon.addPoint(3, -1)
  arrowPolygon.addPoint(-6, -1)

  val midPoint = midpoint(fromPt, toPt)

  val rotate = atan2(toPt.y - fromPt.y, toPt.x - fromPt.x)

  val transform = new AffineTransform()
  transform.translate(midPoint.x, midPoint.y)
  val ptDistance = fromPt distance toPt
  val scale = ptDistance / 12.0 // 12 because it's the length of the arrow polygon.
  transform.scale(scale, scale)
  transform.rotate(rotate)

  val arrow = transform.createTransformedShape(arrowPolygon)

  private def midpoint(p1: Point2D.Double, p2: Point2D.Double) = {
    new Point2D.Double((p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0)
  }
}  
  
//  val rect = new Rectangle2D.Double(lane.x1, lane.y1, lane.length, Constants.laneWidth)
//
//  val transform = new AffineTransform {
//    rotate(lane.rotation, lane.x1, lane.y1)
//  }
//  val outline = transform.createTransformedShape(rect)

  /*
   * The sector that fills the gap between this and the previous roadGraph
   * It's possible that the roads have a different number of lanes. The result is not the most beautiful but should work.
   */
//  private val sect = new Arc2D.Double()
//
//  def sector1 = {
//    val startAng = if (lane.rightIsTouching) {
//      toDegrees(-Constants.angle(lane.startR, lane.startL))
//    } else {
//      toDegrees(-Constants.angle(lane.startL, lane.startR))
//    }
//    //The width of the sector
//    val angExt = if (lane.rightIsTouching) {
//      toDegrees(-Constants.angle(lane.previousRoad.right.endR, lane.previousRoad.left.endL)) - startAng
//    } else {
//      toDegrees(-Constants.angle(lane.previousRoad.left.endL, lane.previousRoad.right.endR)) - startAng
//    }
//    sect.setArcByCenter(lane.touchCorner.getX(), lane.touchCorner.getY(), Constants.laneWidth, startAng, angExt, 2)
//
//    sect
//  }
//  
//  def sector2 = {
//    val startAng = if (lane.nextRoad.rightIsTouching) {
//      toDegrees(-Constants.angle(lane.nextRoad.left.startR, lane.nextRoad.left.startL))
//    } else {
//      toDegrees(-Constants.angle(lane.nextRoad.left.startL, lane.nextRoad.left.startR))
//    }
//    
//    val angExt = if (lane.rightIsTouching) {
//      toDegrees(-Constants.angle(lane.endR, lane.endL)) - startAng
//    } else {
//      toDegrees(-Constants.angle(lane.endL, lane.endR)) - startAng
//    }
//  }