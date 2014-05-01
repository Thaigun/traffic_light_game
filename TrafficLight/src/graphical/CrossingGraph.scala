package graphical

import trafficLogic._
import java.awt.geom.Rectangle2D

class CrossingGraph(crossing: Crossing) {
  val outLine = new Rectangle2D.Double(crossing.sideL.right.getX(), crossing.sideL.right.getY(),  crossing.width, crossing.height)
}