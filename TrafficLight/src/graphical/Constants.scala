package graphical

import java.awt.Color
import java.awt.geom.Point2D
import scala.math._

object Constants {
   val laneWidth = 60
   val trafLineColor = Color.yellow
   val roadColor = Color.LIGHT_GRAY
   
   def angle(a: Point2D.Double, b: Point2D.Double) = {
     if (b.getY-a.getY >= 0) acos((b.getX-a.getX) / (a distance b)) else -acos((b.getX-a.getX) / (a distance b))
   }
}