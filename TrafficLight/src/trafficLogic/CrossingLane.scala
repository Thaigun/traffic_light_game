package trafficLogic

import mapLogic._
import scala.math._
import graphical._
import java.awt.geom.Point2D


/**
 * When FileReader initializes a road, there are cases when the previous or next road of the said new road is a CrossingLane.
 * In such case, the FileReader finds the CrossingLane and asks it where is its ending or starting point. CrossingLane asks the
 * hosting Crossing the same question: what are the coordinates for this road? The Crossing then maintains information of which
 * CrossingLanes and Roads start and end where in that Crossing. 
 */
class CrossingLane(game: Game, id: String, crossing: Crossing, green: Array[Char]) {
  def isEnabled = green.contains(crossing.currentCombo)
  
  def start: Point2D.Double = crossing.getStartingPointFor(id)
  def end: Point2D.Double = crossing.getEndingPointFor(id)
  
  lazy val x1 = start.getX()
  lazy val y1 = start.getY()
  lazy val x2 = end.getX()
  lazy val y2 = end.getY()
  //These have to exist, so we make them null instead of Option. If they don't exist when needed, an exception is wanted.
  var in: Lane = null
  var out: Lane = null
  
  lazy val graphic = new CrossingLaneGraph(this)
  
  def length = start distance end
  def rotation = Constants.angle(start, end)
  
  def startR = new Point2D.Double(x1 + xOffset, y1 + yOffset)
  def startL = new Point2D.Double(x1, y1)
  def previousRoad = in.getRoad
  def nextRoad = out.getRoad
  var rightIsTouching = false
  var leftIsTouching = false
  touchCorner
  
  def touchCorner: Point2D = {
    //The left corner is to be the base case.
      var touching = previousRoad.left.endL
      val rot = Constants.angle(touching, end)
      //If the touching corner is the left corner of this road, that is the start point of this road
      val pr = previousRoad.rotation
      if ((pr > rot && (pr - Pi) < rot)||((rot > pr + Pi)&& rot < pr+2*Pi)) {
        leftIsTouching = true
        touching
      } else { //Otherwise the touching corner is the right corner and therefore we must calculate the position of the left corner.
        rightIsTouching = true
        previousRoad.right.endR
      }
  }
  
  def xOffset = sin(rotation) * Constants.laneWidth
  def yOffset = cos(rotation) * Constants.laneWidth
  
  def getEndingPointFor(road: Road) = {
    crossing.getEndingPointFor(road)
  }
  def getStartingPointFor(road: Road) = {
    crossing.getStartingPointFor(road)
  }
}