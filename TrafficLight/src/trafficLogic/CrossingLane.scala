package trafficLogic

import mapLogic._
import graphical._
import java.awt.geom.Point2D

class CrossingLane(game: Game, id: String, crossing: Crossing, val in: Lane, val out:Lane, green: Array[Char]) {
  
  
  var isGreenLight = false
  var startPoint = null
  var endPoint = null
  
  def start: Point2D.Double = startPoint
  def end: Point2D.Double = endPoint
  
  val graphic = new CrossingLaneGraph(this)
  
}