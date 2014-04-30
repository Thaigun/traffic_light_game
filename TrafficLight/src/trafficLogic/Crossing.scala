package trafficLogic

import scala.collection.mutable.Buffer
import java.awt.geom.Point2D
import graphical._
import scala.math._

  /**
   * Each crossing has a fixed amount of links that roads can link into. The crossing can be thought as a "virtual" rectangle where two roads
   * can connect to each side. One is "in"-road and another can be "out"-road. The length of a side depends on how many _lanes_ are connecting to it. 
   * First side is the right side, then we proceed clockwise.
   */
class Crossing(val id: String, location: Point2D.Double, combinations: Array[Char]) {

  /**
   * @param lessThan: a direction from the center must be less than this value to be in this side
   * @param moreThan: the opposite from lessThan
   * @param side: a character l, r, u or d representing if this is the left, right, down or up side of the crossing, respectively
   */
  class Side(lessThan: Double, moreThan: Double, val side: Char, parent: Crossing) {
    require(Vector('r', 'l', 'u', 'd').contains(side), "Invalid side "+side)
    //Save the CrossingLanes whose in and outgoing lanes are in this side of the crossing.
    var roadO: Option[Road] = None
    var roadI: Option[Road] = None
    def tryAdd(road: Road) = {
      val dirIn = Constants.angle(location, road.start)
      val dirOut = Constants.angle(location, road.end)
      if (moreThan < lessThan) {
        if (dirIn <= lessThan && dirIn >= moreThan) { roadI = Some(road) }
        if (dirOut <= lessThan && dirOut >= moreThan) { roadO = Some(road) }
      } else {
        //If we are in the sector 3 where the directions jump from >0 to <0.
        if (dirIn <= lessThan || dirIn >= moreThan) { roadI = Some(road) }
        if (dirOut <= lessThan || dirOut >= moreThan) { roadO = Some(road) }
      }
      if (roadO.size + roadI.size > 2) throw new Exception("Too many roads are trying to connect to crossing " + road.id)
    }
    def totalLanes = {
      var count = 0
      roadO.foreach(count += _.numOfLanes)
      roadI.foreach(count += _.numOfLanes)
      count
    }
    def height = {
      if (side == 'r' || side == 'l') totalLanes*Constants.laneWidth
      else 0
    }
    def width = {
      if (side == 'u' || side == 'd') totalLanes*Constants.laneWidth
      else 0
    }
    def getInCoord: Point2D.Double = {
      if (side == 'l') {
        val y = location.getY() + totalLanes*Constants.laneWidth / 2 - roadI.get.numOfLanes*Constants.laneWidth
        val x = location.getX() - parent.width / 2
        new Point2D.Double(x, y)
      } else if(side == 'r') {
        val y = location.getY() - totalLanes*Constants.laneWidth / 2 + roadI.get.numOfLanes*Constants.laneWidth
        val x = location.getX() + parent.width / 2
        new Point2D.Double(x, y)
      } else if (side == 'd') {
        val y = location.getY() + parent.height / 2
        val x = location.getX() + totalLanes*Constants.laneWidth / 2 - roadI.get.numOfLanes*Constants.laneWidth
        new Point2D.Double(x, y)
      } else /*side == u*/{
        val y = location.getY() - parent.height / 2
        val x = location.getX() - totalLanes*Constants.laneWidth / 2 + roadI.get.numOfLanes*Constants.laneWidth
        new Point2D.Double(x, y)
      }
    }
    def getOutCoord: Point2D.Double = {
      getInCoord
    }
    def normal: Double = side match {
      case 'l' => Pi
      case 'r' => 0
      case 'u' => -Pi/2
      case 'd' => Pi/2
    }
    def right = side match {
      case 'l' => new Point2D.Double(location.getX()-parent.width/2, location.getY()-parent.height/2)
      case 'r' => new Point2D.Double(location.getX()+parent.width/2, location.getY()+parent.height/2)
      case 'u' => new Point2D.Double(location.getX()+parent.width/2, location.getY()-parent.height/2)
      case 'd' => new Point2D.Double(location.getX()-parent.width/2, location.getY()+parent.height/2)
    }
    def angRightToLeft = side match {
      case 'l' => Pi/2
      case 'r' => -Pi/2
      case 'u' => Pi
      case 'd' => 0
    }
    def angLeftToRight = side match {
      case 'l' => -Pi/2
      case 'r' => Pi/2
      case 'u' => 0
      case 'd' => Pi
    }
  }
  
  def height = max(sideL.height, sideR.height)
  def width = max(sideD.width, sideU.width)
  
  
  var currentCombo: Char = combinations.head
  
  val sideR = new Side(Pi / 4, -Pi / 4, 'r', this)
  val sideD = new Side(Pi * 3 / 4, Pi / 4, 'd', this)
  val sideL = new Side(-Pi * 3 / 4, Pi * 3 / 4, 'l', this)
  val sideU = new Side(-Pi / 4, -Pi * 3 / 4, 'u', this)
  val sides = Vector(sideR, sideD, sideL, sideU)
  def addRoad(road:Road) = sides.foreach(_.tryAdd(road))
  def roadsIn:Array[Road] = sides.foldLeft(Array[Road]())((array, side)=> array ++ side.roadI) 
  def roadsOut:Array[Road] = sides.foldLeft(Array[Road]())((array, side)=>array ++ side.roadO)
 
  val lanes = Buffer[CrossingLane]()
  
  def build = {
    lanes.foreach(_.parseConnection)
  }

  //Gets the touching point for a road, that is, the point where the road touches this crossing
  def getTouchingPointFor(road: Road) = {
    val side = getSide(road).getOrElse(throw new Exception("Road "+road.id+" was not in crossing "+this.id))
    val connect = side.getOutCoord
    val dir = Constants.angle(connect, road.end)
    val rightIsTouching = if (side.side == 'l') dir < 0 else dir > side.normal  
    if (rightIsTouching) {
      road.rightIsTouching = true
      side.right
    } else {
      connect
    }
  }
  //Gets the touching point for a CrossingLane
  def getConnectStartFor(crLane: CrossingLane) = {
    new Point2D.Double()
  }
  //Gets the point where the roads left corner ends
  def getEndingPointFor(road: Road) = {
    new Point2D.Double()
  }
  //Gets the point where the CrossingLanes left corner ends
  def getConnectEndFor(crLane: CrossingLane) = {
    new Point2D.Double()
  }

  def getSide(road: Road) = {
    sides.find(side => side.roadO.isDefined && side.roadO.get == road)
  }

}