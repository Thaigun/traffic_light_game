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
  val cars: Buffer[CarGraph] = game.cars.map(_.graphic)
  val crossings: Array[CrossingGraph] = game.crossings.map(_.graphic)
  val cl: Array[CrossingLane] = game.crossings.foldLeft(Array[CrossingLane]())((array, crossing) => array ++ crossing.lanes)
  val crossingLanes = cl.map(_.graphic)
  
  this.background = Color.WHITE
  
  override def paintComponent(g:Graphics2D) = {
    g.setColor(Color.CYAN)
    g.setBackground(Color.PINK)
    
    for (road <- roads) {
      g.setColor(Constants.roadColor)
      g.fill(road.outline)
      g.fill(road.sector)
      g.fill(road.endSect)
      g.setColor(Constants.trafLineColor)
      road.trafLines.foreach(g.draw(_))
      if (road.road.hasNextCross) road.endSectLines.foreach(g.draw(_)) 
      if (road.road.hasPrevRoad || road.road.hasPrevCross) road.sectorLines.foreach(g.draw(_))
    }
    
    
    for (crossing <- crossings) {
      g.setColor(Constants.roadColor)
      g.fill(crossing.outLine)
      g.setColor(Constants.trafLineColor)
      g.draw(crossing.outLine)
    }
    
    g.setColor(Color.GREEN)
    for (lane <- crossingLanes) {
      g.setColor(Color.GREEN)
      g.draw(lane.arrow)
    }
    
    for (car <- cars) {
      g.setColor(car.color)
      g.fill(car.outline)
    }
//    import java.awt.geom.Rectangle2D
//    g.draw(new Rectangle2D.Double(600,390, 2, 2))
//    g.draw(new Rectangle2D.Double(630,360, 2, 2))
  }
  
  def run = {
    while (!game.over) {
      println("in the run method of game panel")
      this.repaint 
    }  
  }
}