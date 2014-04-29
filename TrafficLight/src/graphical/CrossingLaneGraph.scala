package graphical

import trafficLogic._
import java.awt.geom.Arc2D;
import java.awt.geom.AffineTransform
import java.awt.geom.Rectangle2D
import java.awt.Shape
import java.awt.geom.Line2D
import scala.swing._
import scala.math._

class CrossingLaneGraph(lane: CrossingLane) {
  val rect = new Rectangle2D.Double(lane.x1, lane.y1, lane.length, Constants.laneWidth)

  val transform = new AffineTransform {
    rotate(lane.rotation, lane.x1, lane.y1)
  }
  val outline = transform.createTransformedShape(rect)

  /*
   * The sector that fills the gap between this and the previous roadGraph
   * It's possible that the roads have a different number of lanes. The result is not the most beautiful but should work.
   */
  private val sect = new Arc2D.Double()

  def sector = {
    val startAng = if (lane.rightIsTouching) {
      toDegrees(-Constants.angle(lane.startR, lane.startL))
    } else {
      toDegrees(-Constants.angle(lane.startL, lane.startR))
    }
    //The width of the sector
    val angExt = if (lane.rightIsTouching) {
      toDegrees(-Constants.angle(lane.previousRoad.right.endR, lane.previousRoad.left.endL)) - startAng
    } else {
      toDegrees(-Constants.angle(lane.previousRoad.left.endL, lane.previousRoad.right.endR)) - startAng
    }
    sect.setArcByCenter(lane.touchCorner.getX(), lane.touchCorner.getY(), Constants.laneWidth, startAng, angExt, 2)

    sect
  }

}