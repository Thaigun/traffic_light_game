package graphical

import java.awt.Color
import java.awt.geom.Point2D
import scala.math._

object Constants {
   val laneWidth = 40
   val trafLineColor = Color.yellow
   val roadColor = Color.LIGHT_GRAY
   val maxSpeed = 180
   val crossWidth = 2 * laneWidth
   
   
   val preferredGap = 7
   
   /*From file*/
   val carNumber = 10
   val goal = 10
   /*--------*/
   
   def angle(a: Point2D.Double, b: Point2D.Double) = {
     if (b.getY-a.getY >= 0) acos((b.getX-a.getX) / (a distance b)) else -acos((b.getX-a.getX) / (a distance b))
   }
   
   def carColor = Color.blue
   
   
}