package graphical

import trafficLogic._
import java.awt.geom.RoundRectangle2D

class CarGraph(car: Car) {
  val color = Constants.carColor
  val outline = new RoundRectangle2D.Double(car.x, car.y, car.length, car.width, car.length/5, car.width/5)
}