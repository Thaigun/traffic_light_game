package mapLogic

import java.awt.Color
import scala.util.Random
import java.awt.geom.Point2D
import scala.math._
import trafficLogic._

object Constants {
   val laneWidth = 30
   val trafLineColor = Color.yellow
   val roadColor = Color.LIGHT_GRAY
   val maxSpeed = 100
   val crossWidth = 2 * laneWidth
   
   
   val preferredGap = laneWidth / 3.5
   
   /*From file*/
   val carNumber = 35
   /*--------*/
   
   def angle(a: Point2D.Double, b: Point2D.Double): Double = {
     if ((a distance b) == 0.0) return 0
     if (b.getY-a.getY >= 0) acos((b.getX-a.getX) / (a distance b)) else -acos((b.getX-a.getX) / (a distance b))
   }
   
   def carColor = {
     val colors = Array(Color.BLACK, Color.BLUE, Color.RED, Color.DARK_GRAY, Color.YELLOW, Color.PINK, Color.ORANGE, Color.MAGENTA)
     colors(new Random().nextInt(colors.size - 1))
   }
   
   def getRoadWeighted(roads: Array[Road]) = {
     val totalSum = roads.map(_.weight).sum
     val rand = new scala.util.Random()
     val hit = rand.nextInt(totalSum)
     
     var holder = roads(0).weight - 1
     var i = 0
     while (holder < hit) {
       i += 1
       holder += roads(i).weight
       
     }
     roads(i)
     
   }
   
   val redLight = Color.red
   val greenLight = Color.green
   
}