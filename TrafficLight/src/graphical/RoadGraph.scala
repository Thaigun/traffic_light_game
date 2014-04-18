package graphical

import java.awt.geom.AffineTransform
import java.awt.geom.Rectangle2D
import java.awt.Shape
import scala.swing._
import scala.math._

class RoadGraph(startX: Int, startY: Int)(endX: Int, endY: Int){
  val length = sqrt(pow((endX - startX), 2) + pow((endY - startY), 2)).toFloat
  val rect = new Rectangle2D.Float(startX, startY, length, Constants.roadWidth)
  val transform = new AffineTransform {
    val theta = math.acos((endX-startX)/length)
    rotate(theta, startX, startY)
  }
  val shape = transform.createTransformedShape(rect)

}