package graphical

import trafficLogic._
import mapLogic._
import java.awt.Polygon
import java.awt.geom.AffineTransform
import java.awt.geom.Point2D
import java.awt.geom.Line2D
import scala.math._

class CrossingLaneGraph(lane: CrossingLane) {
  
  def fromPt = lane.start
  def toPt = lane.end  

  val arrow = new Line2D.Double(fromPt, toPt)

  
  def color = if (lane.isEnabled) Constants.greenLight else Constants.redLight
}  