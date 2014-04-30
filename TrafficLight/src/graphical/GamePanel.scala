package graphical

import mapLogic._
import trafficLogic._
import scala.collection.mutable.Buffer
import scala.swing._
import java.awt.Color
import scala.swing.event._
import scala.swing.event.Key._
import java.awt.geom.Line2D

class GamePanel(game: Game) extends Panel {
  this.requestFocusInWindow
  val roads: Array[RoadGraph] = game.roads.map(_.graphic)
  val cars: Array[CarGraph] = game.cars.map(_.graphic)
  val crossings: Array[CrossingGraph] = game.crossings.map(_.graphic)
  val cl: Array[CrossingLane] = game.crossings.foldLeft(Array[CrossingLane]())((array, crossing) => array ++ crossing.lanes)
  val crossingLanes = cl.map(_.graphic)
  
  override def paintComponent(g:Graphics2D) = {
    
    for (road <- roads) {
      g.setColor(Constants.roadColor)
      g.fill(road.outline)
      g.fill(road.sector)
      g.setColor(Constants.trafLineColor)
      road.trafLines.foreach(g.draw(_))
      if (road.road.hasPrevRoad) road.sectorLines.foreach(g.draw(_))
    }
    
    g.setColor(Color.red)
    for (crossing <- crossings) {
      g.draw(crossing.outLine)
    }
    
    g.setColor(Color.GREEN)
    for (lane <- crossingLanes) {
      g.setColor(Color.GREEN)
      g.draw(lane.arrow)
    }
  }
}