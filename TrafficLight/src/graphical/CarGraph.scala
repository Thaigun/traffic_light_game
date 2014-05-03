package graphical

import trafficLogic._
import java.awt.geom.AffineTransform
import java.awt.geom.RoundRectangle2D

class CarGraph(val car: Car) {
  val color = Constants.carColor

  def outline = {
    val rect = new RoundRectangle2D.Double(car.cornerLocation.getX(), car.cornerLocation.getY(), car.length, car.width, car.length / 5, car.width / 5)
    val transform = new AffineTransform {
      rotate(car.direction, car.cornerLocation.getX(), car.cornerLocation.getY())
    }
    transform.createTransformedShape(rect)
  }

}