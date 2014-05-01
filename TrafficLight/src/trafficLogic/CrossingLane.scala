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
  
  def previousRoad = in.getRoad
  def nextRoad = out.getRoad
  
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