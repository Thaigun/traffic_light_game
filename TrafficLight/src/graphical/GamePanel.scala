package graphical

import mapLogic._
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
  
  override def paintComponent(g:Graphics2D) = {
    g.setColor(Color.LIGHT_GRAY)
    for (road <- roads) {
      g.fill(road.shape)
    }
  }
}