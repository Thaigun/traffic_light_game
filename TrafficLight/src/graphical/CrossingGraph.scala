package graphical

import trafficLogic._
import java.awt.geom.Rectangle2D

class CrossingGraph(crossing: Crossing) {
  val outLine = new Rectangle2D.Double(crossing.location.getX-crossing.width/2, crossing.location.getY-crossing.height/2, crossing.width, crossing.height)
}