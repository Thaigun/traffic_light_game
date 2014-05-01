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
  transform.scale(scale, 2)
  transform.rotate(rotate)

  val arrow = transform.createTransformedShape(arrowPolygon)

  private def midpoint(p1: Point2D.Double, p2: Point2D.Double) = {
    new Point2D.Double((p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0)
  }
}  