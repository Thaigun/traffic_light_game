package graphical

import trafficLogic._
import java.awt.geom.Point2D
import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon

class CrossingButton(crossing: Crossing, combo: Char, val point: Point2D.Double, imgSrc: String) extends Button() {
  action = new Action("Toggle") {
    def apply = crossing.currentCombo = combo
  }
  icon = new ImageIcon(imgSrc)
}