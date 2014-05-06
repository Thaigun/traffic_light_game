package trafficLogic

import scala.collection.mutable.Buffer
import java.awt.geom.Point2D
import java.awt.geom.Arc2D
import graphical._
import scala.math._
import mapLogic.Constants
import javax.swing._
import java.awt.event.ActionEvent

/**
 * Each crossing has a fixed amount of links that roads can link into. The crossing can be thought as a "virtual" rectangle where two roads
 * can connect to each side. One is "in"-road and another can be "out"-road. The length of a side depends on how many _lanes_ are connecting to it.
 * First side is the right side, then we proceed clockwise.
 */
class Crossing(val id: String, val location: Point2D.Double, combinations: Array[Char]) {

  /**
   * @param lessThan: a direction from the center must be less than this value to be in this side
   * @param moreThan: the opposite from lessThan
   * @param side: a character l, r, u or d representing if this is the left, right, down or up side of the crossing, respectively
   */
  class Side(lessThan: Double, moreThan: Double, val side: Char, parent: Crossing) {
    require(Vector('r', 'l', 'u', 'd').contains(side), "Invalid side " + side)
    //Save the CrossingLanes whose in and outgoing lanes are in this side of the crossing.
    var roadO: Option[Road] = None
    var roadI: Option[Road] = None
    def tryAddIn(road: Road) = {
      val dirIn = if (road.hasPrevRoad) {
        Constants.angle(location, road.previousRoad.get.endM)
      } else {
        Constants.angle(location, road.start)
      }
      if (moreThan < lessThan) {
        if (dirIn <= lessThan && dirIn >= moreThan) { roadI = Some(road) }
      } else {
        //If we are in the side 'l' where the directions jump from >0 to <0.
        if (dirIn <= lessThan || dirIn >= moreThan) { roadI = Some(road) }
      }
      if (roadO.size + roadI.size > 2) throw new Exception("Too many roads are trying to connect to crossing " + road.id)
    }
    def tryAddOut(road: Road) = {
      val dirOut = Constants.angle(location, road.end)
      if (moreThan < lessThan) {
        if (dirOut <= lessThan && dirOut >= moreThan) { roadO = Some(road) }
      } else {
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
      if (side == 'r' || side == 'l') totalLanes * Constants.laneWidth
      else 0
    }
    def width = {
      if (side == 'u' || side == 'd') totalLanes * Constants.laneWidth
      else 0
    }
    def getInCoord: Point2D.Double = {
      if (this.roadI.isEmpty) return this.left
      if (side == 'l') {
        val y = location.getY() + totalLanes * Constants.laneWidth / 2 - roadI.get.numOfLanes * Constants.laneWidth
        val x = location.getX() - parent.width / 2
        new Point2D.Double(x, y)
      } else if (side == 'r') {
        val y = location.getY() - totalLanes * Constants.laneWidth / 2 + roadI.get.numOfLanes * Constants.laneWidth
        val x = location.getX() + parent.width / 2
        new Point2D.Double(x, y)
      } else if (side == 'd') {
        val y = location.getY() + parent.height / 2
        val x = location.getX() + totalLanes * Constants.laneWidth / 2 - roadI.get.numOfLanes * Constants.laneWidth
        new Point2D.Double(x, y)
      } else /*side == u*/ {
        val y = location.getY() - parent.height / 2
        val x = location.getX() - totalLanes * Constants.laneWidth / 2 + roadI.get.numOfLanes * Constants.laneWidth
        new Point2D.Double(x, y)
      }
    }
    def getOutCoord: Point2D.Double = {
      if (this.roadO.isEmpty) return this.getInCoord
      if (side == 'l') {
        val y = location.getY() - totalLanes * Constants.laneWidth / 2 + roadO.get.numOfLanes * Constants.laneWidth
        val x = location.getX() - parent.width / 2
        new Point2D.Double(x, y)
      } else if (side == 'r') {
        val y = location.getY() + totalLanes * Constants.laneWidth / 2 - roadO.get.numOfLanes * Constants.laneWidth
        val x = location.getX() + parent.width / 2
        new Point2D.Double(x, y)
      } else if (side == 'd') {
        val y = location.getY() + parent.height / 2
        val x = location.getX() - totalLanes * Constants.laneWidth / 2 + roadO.get.numOfLanes * Constants.laneWidth
        new Point2D.Double(x, y)
      } else /*side == u*/ {
        val y = location.getY() - parent.height / 2
        val x = location.getX() + totalLanes * Constants.laneWidth / 2 - roadO.get.numOfLanes * Constants.laneWidth
        new Point2D.Double(x, y)
      }
    }
    def normal: Double = side match {
      case 'l' => Pi
      case 'r' => 0
      case 'u' => -Pi / 2
      case 'd' => Pi / 2
    }
    def right = side match {
      case 'l' => new Point2D.Double(location.getX() - parent.width / 2, location.getY() - parent.height / 2)
      case 'r' => new Point2D.Double(location.getX() + parent.width / 2, location.getY() + parent.height / 2)
      case 'u' => new Point2D.Double(location.getX() + parent.width / 2, location.getY() - parent.height / 2)
      case 'd' => new Point2D.Double(location.getX() - parent.width / 2, location.getY() + parent.height / 2)
    }
    def left = side match {
      case 'l' => new Point2D.Double(location.getX() - parent.width / 2, location.getY() + parent.height / 2)
      case 'r' => new Point2D.Double(location.getX() + parent.width / 2, location.getY() - parent.height / 2)
      case 'u' => new Point2D.Double(location.getX() - parent.width / 2, location.getY() - parent.height / 2)
      case 'd' => new Point2D.Double(location.getX() + parent.width / 2, location.getY() + parent.height / 2)
    }
    def angRightToLeft = side match {
      case 'l' => Pi / 2
      case 'r' => -Pi / 2
      case 'u' => Pi
      case 'd' => 0
    }
    def angLeftToRight = side match {
      case 'l' => -Pi / 2
      case 'r' => Pi / 2
      case 'u' => 0
      case 'd' => Pi
    }
    //This method returns a point moved to right the amount of pixels given
    def moveR(point: Point2D.Double, howMuch: Double) = side match {
      case 'l' => new Point2D.Double(point.x, point.y - howMuch)
      case 'r' => new Point2D.Double(point.x, point.y + howMuch)
      case 'u' => new Point2D.Double(point.x + howMuch, point.y)
      case 'd' => new Point2D.Double(point.x - howMuch, point.y)
    }

  }

  def height = max(Constants.laneWidth, max(sideL.height, sideR.height))
  def width = max(Constants.laneWidth, max(sideD.width, sideU.width))

  /*
   * The previousCombo is used by a CrossingLane to determine whether it should remain enabled during light-change. This occurs
   * when the previousCombo is defined and the lane is enabled in that and the current combo. If the lane is not in either of them,
   * it is disabled as long as the previousCOmbo is defined.
   */
  var previousCombo: Option[Char] = None
  private var currentCombo: Char = combinations.head
  def getCurrentCombo = currentCombo
  def setNewCombo(c: Char) = {
    previousCombo = Some(currentCombo)
    currentCombo = c
    val timerAction = new AbstractAction() {
      def actionPerformed(event: ActionEvent): Unit = previousCombo = None
    }
    val timer = new Timer(2000, timerAction) {
      setRepeats(false)
      start
    }
  }

  val sideR = new Side(Pi / 4, -Pi / 4, 'r', this)
  val sideD = new Side(Pi * 3 / 4, Pi / 4, 'd', this)
  val sideL = new Side(-Pi * 3 / 4, Pi * 3 / 4, 'l', this)
  val sideU = new Side(-Pi / 4, -Pi * 3 / 4, 'u', this)
  val sides = Vector(sideR, sideD, sideL, sideU)
  def addRoadIn(road: Road) = sides.foreach(_.tryAddIn(road))
  def addRoadOut(road: Road) = sides.foreach(_.tryAddOut(road))
  def roadsIn: Array[Road] = sides.foldLeft(Array[Road]())((array, side) => array ++ side.roadI)
  def roadsOut: Array[Road] = sides.foldLeft(Array[Road]())((array, side) => array ++ side.roadO)
  val fillSectors = Buffer[Arc2D]()
  val lanes = Buffer[CrossingLane]()
  lazy val graphic = new CrossingGraph(this)

  def build = {
    lanes.foreach(_.parseConnection)
  }

  //Gets the touching point for an outgoing road, that is, the point where the road touches this crossing
  def getTouchingPointFor(road: Road) = {
    val side = getSide(road).getOrElse(throw new Exception("Road " + road.id + " was not in crossing " + this.id))
    val connect = side.getOutCoord
    val dir = Constants.angle(connect, road.end)
    val rightIsTouching = if (side.side == 'l') dir < 0 else dir > side.normal
    if (rightIsTouching) {
      road.rightIsTouching = true
      side.moveR(side.getOutCoord, road.numOfLanes * Constants.laneWidth)
    } else {
      road.leftIsTouching = true
      connect
    }
  }

  def getTouchingEndPointFor(road: Road) = {
    val side = getSide(road).getOrElse(throw new Exception("Road " + road.id + " was not in crossing " + this.id))
    if (rightEndIsTouching(road)) {
      side.moveR(side.getInCoord, -road.numOfLanes * Constants.laneWidth)
    } else {
      side.getInCoord
    }
  }

  //Gets the point where the roads left corner ends
  def getEndingPointFor(road: Road): Point2D.Double = {
    val side = getSide(road).getOrElse(throw new Exception("Road " + road.id + " was not in crossing " + this.id))
    val rightCornerTouch = rightEndIsTouching(road)
    if (!rightCornerTouch) side.getInCoord
    else {
      //      if (side.side == 'l') {
      val touchPoint = side.moveR(side.getInCoord, -road.numOfLanes * Constants.laneWidth)
      val alfa = Constants.angle(road.start, touchPoint)
      val theta = acos((Constants.laneWidth * road.numOfLanes) / (road.start distance touchPoint))
      val sigma = Pi / 2 - alfa - theta
      val leftCorner = new Point2D.Double(touchPoint.getX - sin(sigma) * Constants.laneWidth * road.numOfLanes, touchPoint.getY - cos(sigma) * Constants.laneWidth * road.numOfLanes)
      leftCorner
      //      } else if (side.side == 'r') {
      //        
      //      } else if (side.side == 'u') {
      //
      //      } else /*side.side=='d'*/ {
      //
      //      }
    }
  }

  //Gets the starting point for a CrossingLane
  def getConnectStartFor(crLane: CrossingLane) = {
    val side = getSide(crLane.in.getRoad).getOrElse(throw new Exception("Did not find the right side of a crossing."))
    side.moveR(side.getInCoord, -Constants.laneWidth * (crLane.in.laneNumber + 0.5))
  }
  //Gets the point where the CrossingLanes end
  def getConnectEndFor(crLane: CrossingLane) = {
    val side = getSide(crLane.out.getRoad).getOrElse(throw new Exception("Did not find the right side of a crossing."))
    side.moveR(side.getOutCoord, Constants.laneWidth * (crLane.out.laneNumber + 0.5))
  }

  def getSide(road: Road) = {
    val tryOut = sides.find(side => side.roadO.isDefined && side.roadO.get == road)
    if (tryOut.isEmpty) sides.find(side => side.roadI.isDefined && side.roadI.get == road) else tryOut
  }

  def rightEndIsTouching(road: Road) = {
    val side = getSide(road).getOrElse(throw new Exception("Road " + road.id + " was not in crossing " + this.id))
    val tempAngle = if (road.hasPrevRoad) {
      Constants.angle(middlePointOfIncoming(road), road.previousRoad.get.endM)
    } else {
      Constants.angle(side.getInCoord, road.start)
    }
    if (side.side == 'l') tempAngle > 0 else tempAngle < side.normal
  }

  def middlePointOfIncoming(road: Road) = {
    val side = getSide(road).getOrElse(throw new Exception("Road " + road.id + " was not in crossing " + this.id))
    side.moveR(side.getInCoord, -0.5 * road.numOfLanes * Constants.laneWidth)
  }

  def laneFromTo(from: Lane, to: Road) = {
    lanes.find(lane => lane.in == from && lane.out.getRoad == to)
  }

  override def toString = "Crossing id " + id

}