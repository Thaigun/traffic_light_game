package mapLogic

import java.awt.Color
import java.awt.geom.Point2D
import scala.math._
import trafficLogic._

object Constants {
   val laneWidth = 40
   val trafLineColor = Color.yellow
   val roadColor = Color.LIGHT_GRAY
   val maxSpeed = 100
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
     
   
   
   
}