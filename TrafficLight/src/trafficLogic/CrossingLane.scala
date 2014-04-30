package trafficLogic

import mapLogic._
import scala.math._
import graphical._
import java.awt.geom.Point2D


/**
 * @param id: An identifying string for this CrossingLane
 * @param crossing: The host crossing that this lane belongs to
 * @param green: An array of characters representing different combinations when this lane is enabled
 * @param connection: A string that describes which road this lane connects to which other road.
 */
class CrossingLane(val id: String, val crossing: Crossing, green: Array[Char], connection: String) {
  def isEnabled = green.contains(crossing.currentCombo)
  
  lazy val start = findStart
  def findStart: Point2D.Double = crossing.getConnectStartFor(this)
  lazy val end = findEnd
  def findEnd: Point2D.Double = crossing.getConnectEndFor(this)
  
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
  
//  def startR = new Point2D.Double(x1 + xOffset, y1 + yOffset)
//  def startL = new Point2D.Double(x1, y1)
//  def endL = end
//  def endR = new Point2D.Double(endL.getX()+xOffset, endL.getY()+yOffset)
  
  def previousRoad = in.getRoad
  def nextRoad = out.getRoad
//  var rightIsTouching = false
//  var leftIsTouching = false
  
//  def touchCorner: Point2D = {
//    //The left corner is to be the base case.
//      var touching = previousRoad.left.endL
//      val rot = Constants.angle(touching, end)
//      //If the touching corner is the left corner of this road, that is the start point of this road
//      val pr = previousRoad.rotation
//      if ((pr > rot && (pr - Pi) < rot)||((rot > pr + Pi)&& rot < pr+2*Pi)) {
//        leftIsTouching = true
//        touching
//      } else { //Otherwise the touching corner is the right corner and therefore we must calculate the position of the left corner.
//        rightIsTouching = true
//        previousRoad.right.endR
//      }
//  }
  
//  def xOffset = sin(rotation) * Constants.laneWidth
//  def yOffset = cos(rotation) * Constants.laneWidth
  
//  def getEndingPointFor(road: Road) = {
//    crossing.getEndingPointFor(road)
//  }
//  def getStartingPointFor(road: Road) = {
//    crossing.getTouchingPointFor(road)
//  }
  
  def parseConnection = {
    val inS = connection.split("->")(0)
    val outS = connection.split("->")(1)
    val inId = inS.split('.')(0)
    val inLaneS = inS.split('.')(1)
    val outId = outS.split('.')(0)
    val outLaneS = outS.split('.')(1)
    
    val inRoad = crossing.roadsIn.find(_.id == inId).getOrElse(throw new Exception("Something went wrong when trying to parse CrossingLane connection string"))
    val inLane = inRoad(inLaneS.toInt).getOrElse(throw new Exception("Something went wrong when trying to parse CrossingLane connection string"))
    
    val outRoad = crossing.roadsOut.find(_.id == outId).getOrElse(throw new Exception("Something went wrong when trying to parse CrossingLane connection string"))
    val outLane = outRoad(outLaneS.toInt).getOrElse(throw new Exception("Something went wrong when trying to parse CrossingLane connection string"))
    
    this.in = inLane
    this.out = outLane
    
  }
}