package graphical

import trafficLogic._
import java.awt.geom.Point2D
import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon

class CrossingButton(crossing: Crossing, combo: Char, val point: Point2D.Double, imgSrc: String) extends Label {
  reactions += {
    case click: MousePressed => changeCombo(click.point)
  }
  
  icon = new ImageIcon(imgSrc)
  def getIcon = icon.asInstanceOf[ImageIcon]
  
  def changeCombo(where: java.awt.Point) {
    if (where.getX() >= point.getX() && where.getX() <= point.getX() + icon.getIconWidth()
        && where.getY() >= point.getY() && where.getY() <= point.getY() + icon.getIconHeight()) {
      crossing.setNewCombo(combo)
    }
  }
}