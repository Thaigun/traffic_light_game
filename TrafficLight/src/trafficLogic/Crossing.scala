package trafficLogic

import scala.collection.mutable.Buffer
import java.awt.geom.Point2D
import graphical._
import scala.math._

class Crossing(id: String, location: Point2D.Double, combinations: Array[Char]) {
  val lanes = Buffer[CrossingLane]()
  def addLane(lane: CrossingLane) = lanes += lane
  
  /*
   * The lane that has been marked with an asterisk(an arbitary if there are multiple). One side of the crossing is heading towards the
   * incoming lane of that CrossingLane
   */
  
  def build = {
    
  }
  
}