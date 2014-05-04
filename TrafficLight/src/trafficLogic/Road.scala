package trafficLogic

import mapLogic._
import java.awt.geom.Point2D
import graphical._
import scala.math._

class Road(val game: Game, val id: String, val numOfLanes: Int)(startX: Int, startY: Int)(private val x2: Int, private val y2: Int) {
  def apply(n: Int): Option[Lane] = if (n >= 0 && lanes.size >= n) Some(lanes(n)) else None
  def right: Lane = this(numOfLanes - 1).get
  def left: Lane = this(0).get

  /*
   * If the previous road is defined and not a crossing, the starting coordinated depend on them, otherwise the are the same as
   * the ones given as constructor parameters.
   */
  lazy val start = findStart
  def findStart: Point2D.Double = {
    val touchingCorner = touchCorner.getOrElse(return new Point2D.Double(startX, startY))
    if (leftIsTouching) {
      touchingCorner
    } else {
      if (!hasNextCross) {
        val theta = Constants.angle(touchingCorner, new Point2D.Double(x2, y2))
        val alfa = asin((Constants.laneWidth * numOfLanes) / (touchingCorner distance new Point2D.Double(x2, y2)))
        val sigma = theta + alfa
        val leftCorner = new Point2D.Double(touchingCorner.getX() + sin(sigma) * Constants.laneWidth * numOfLanes, touchingCorner.getY() - cos(sigma) * Constants.laneWidth * numOfLanes)
        leftCorner
      } else {
        if (nextCrossing.get.rightEndIsTouching(this)) {
          val sigma = Constants.angle(touchingCorner, nextCrossing.get.getEndingPointFor(this)) - Pi
          new Point2D.Double(touchingCorner.getX + cos(sigma) * Constants.laneWidth * numOfLanes, touchingCorner.getY + sin(sigma) * Constants.laneWidth * numOfLanes)
        } else {
          val theta = Constants.angle(touchingCorner, nextCrossing.get.getEndingPointFor(this))
          val alfa = asin((Constants.laneWidth * numOfLanes) / (touchingCorner distance new Point2D.Double(x2, y2)))
          val sigma = theta + alfa
          val leftCorner = new Point2D.Double(touchingCorner.getX() + sin(sigma) * Constants.laneWidth * numOfLanes, touchingCorner.getY() - cos(sigma) * Constants.laneWidth * numOfLanes)
          leftCorner
        }
      }
    }
  }

  lazy val end = findEnd
  def findEnd: Point2D.Double = {
    if (!hasNextCross) {
      new Point2D.Double(x2, y2)
    } else {
      nextCrossing.get.getEndingPointFor(this)
    }
  }

  lazy val lanes = Array.tabulate(numOfLanes)((n: Int) => new Lane(this, n))
  var previousCrossing: Option[Crossing] = None
  var nextCrossing: Option[Crossing] = None
  var previousRoad: Option[Road] = None
  var nextRoad: Option[Road] = None
  lazy val x1 = start.getX()
  lazy val y1 = start.getY()
  lazy val graphic = new RoadGraph(this)

  //We want to construct the starting point instead of just using start method to avoid computing it all over again.
  def rotation = Constants.angle(start, end)
  def length = start distance end
  def hasNextCross = nextCrossing.isDefined
  def hasPrevCross = previousCrossing.isDefined
  def hasNextRoad = nextRoad.isDefined
  def hasPrevRoad = previousRoad.isDefined
  var rightIsTouching: Boolean = false
  var leftIsTouching: Boolean = false

  def touchCorner: Option[Point2D.Double] = {
    if (!hasPrevRoad && !hasPrevCross) {
      None
    } else if (!hasPrevCross && !hasNextCross) {
      //The left corner is to be the base case.
      var touching = previousRoad.get.left.endL
      val rot = Constants.angle(touching, end)
      //If the touching corner is the left corner of this road, that is the start point of this road
      val pr = previousRoad.get.rotation
      if ((pr > rot && (pr - Pi) < rot) || ((rot > pr + Pi) && rot < pr + 2 * Pi)) {
        leftIsTouching = true
        Some(touching)
      } else { //Otherwise the touching corner is the right corner and therefore we must calculate the position of the left corner.
        rightIsTouching = true
        Some(previousRoad.get.right.endR)
      }
    } else if (!hasPrevCross && hasNextCross) {
      val leftTouches = (previousRoad.get.left.endL distance nextCrossing.get.middlePointOfIncoming(this)) <=
        (previousRoad.get.right.endR distance nextCrossing.get.middlePointOfIncoming(this))
      leftIsTouching = leftTouches
      rightIsTouching = !leftTouches
      if (leftTouches) Some(previousRoad.get.left.endL) else Some(previousRoad.get.right.endR)
    } else {
      //When the previous road is a crossing
      Some(previousCrossing.get.getTouchingPointFor(this))
    }
  }

  def startM = {
    if (numOfLanes % 2 == 0) apply(numOfLanes / 2).get.startL
    else apply(numOfLanes / 2).get.startM
  }
  def endM = {
    if (numOfLanes % 2 == 0) apply(numOfLanes / 2).get.endL
    else apply(numOfLanes / 2).get.endM
  }

  def whichLaneFor(current: Lane, road: Road): Option[Lane] = {
    if (!hasNextRoad && !hasNextCross) return None
    if (hasNextRoad && nextRoad.get == road) {
      if (this.numOfLanes <= road.numOfLanes) Some(current)
      else {
        if (current.isLeft) current.laneRight else current.laneLeft
      }
    } else if (hasNextRoad) {
      None
    } else {
      val crossing = nextCrossing.getOrElse(throw new Exception("Cannot find a crossingLane because there's no next Crossing"))
      val crossingLane = crossing.lanes.find(_.out.getRoad == road).getOrElse(throw new Exception("The desired road " + road.id + " is not accessible from this road: " + id))
      this.lanes.find(_ == crossingLane.in)
    }
  }

  //TODO: Also if they are bigger than the map!
  def startsFromEdge = !hasPrevRoad && !hasPrevCross// && (startX <= 0 || startY <= 0 || startX >= game.size._1 || startY >= game.size._2)
  def endsToEdge = (!hasNextRoad && !hasNextCross)//  (end.getX <= 0 || end.getY <= 0 || end.getX() >= game.size._1 || end.getY() >= game.size._2)
}