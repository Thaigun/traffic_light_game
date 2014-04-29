package trafficLogic

import mapLogic._
import java.awt.geom.Point2D
import graphical._
import scala.math._

class Road(val game: Game, val id: String, val numOfLanes: Int)(private val startX: Int, private val startY: Int)(val x2: Int, val y2: Int) {

  def apply(n: Int): Option[Lane] = if (n >= 0 && lanes.size >= n) Some(lanes(n)) else None
  def right: Lane = this(numOfLanes - 1).get
  def left: Lane = this(0).get

  /*
   * If the previous road is defined and not a crossing, the starting coordinated depend on them, otherwise the are the same as
   * the ones given as constructor parameters.
   */
  def start: Point2D.Double = {
    val touchingCorner = touchCorner.getOrElse(return new Point2D.Double(startX, startY))
    if (touchingCorner equals previousRoad.get.left.endL) {
      touchingCorner
    } else {
      //Calculate the direction of this road knowing the right starting corner and the left ending corner.
      //Theta is the angle (horizontal be 0) of a line segment drawn from the left right start to the left end.
      //Alfa is the angle between the line described above and the right side of the road. Alfa+theta is the total rotation sigma
      val theta = Constants.angle(touchingCorner, new Point2D.Double(x2, y2))
      val alfa = asin((Constants.laneWidth * numOfLanes) / (touchingCorner distance new Point2D.Double(x2, y2)))
      val sigma = theta + alfa
      val leftCorner = new Point2D.Double(touchingCorner.getX() + sin(sigma) * Constants.laneWidth * numOfLanes, touchingCorner.getY() - cos(sigma) * Constants.laneWidth * numOfLanes)
      leftCorner
    }
  }

  def end: Point2D.Double = {
    new Point2D.Double(x2, y2)
  }

  lazy val lanes = Array.tabulate(numOfLanes)((n: Int) => new Lane(this, n))
  var previousRoad: Option[Road] = None
  var nextRoad: Option[Road] = None
  lazy val x1 = start.getX()
  lazy val y1 = start.getY()
  lazy val graphic = new RoadGraph(this)

  def rotation = Constants.angle(start, end)
  def length = start distance end
  def hasNext = nextRoad.isDefined
  def hasPrevious = previousRoad.isDefined
  var rightIsTouching: Boolean = false
  var leftIsTouching: Boolean = false
  //This makes sure that rightIsTouching and leftIsTouching are having correct values 
  touchCorner
  
  def touchCorner: Option[Point2D.Double] = {
    if (!hasPrevious) {
      None
    } else {
      //The left corner is to be the base case.
      var touching = previousRoad.get.left.endL
      val rot = Constants.angle(touching, end)
      //If the touching corner is the left corner of this road, that is the start point of this road
      val pr = previousRoad.get.rotation
      if ((pr > rot && (pr - Pi) < rot)||((rot > pr + Pi)&& rot < pr+2*Pi)) {
        leftIsTouching = true
        Some(touching)
      } else { //Otherwise the touching corner is the right corner and therefore we must calculate the position of the left corner.
        rightIsTouching = true
        Some(previousRoad.get.right.endR)
      }
    }
  }
}