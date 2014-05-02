package graphical

import trafficLogic._
import java.awt.geom.AffineTransform
import java.awt.geom.RoundRectangle2D

class CarGraph(car: Car) {
  val color = Constants.carColor

  private val rect = new RoundRectangle2D.Double(car.x, car.y, car.length, car.width, car.length / 5, car.width / 5)
  private val transform = new AffineTransform

  def outline = {
    rect.x = car.x
    rect.y = car.y
    transform.rotate(car.direction, car.cornerLocation.getX(), car.cornerLocation.getY())
    transform.createTransformedShape(rect)    
  }

}