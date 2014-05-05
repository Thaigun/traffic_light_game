package trafficLogic

import java.awt.geom.Point2D

/**
 * This class represents a navigating point that cars head to. It does not implement any steering behavior or other calculation
 * but is used by the Game-class to define new navigating goals (nextLeg in Car).
 */
class NavGoal(x: Double, y: Double, var onRoad: Road) {
  var position = new Point2D.Double(x, y)
  var kind: NavGoal.Goal = NavGoal.roadEnd
}

object NavGoal extends Enumeration {
  def apply(pt: Point2D.Double, road: Road) = new NavGoal(pt.getX(), pt.getY(), road)
  
  type Goal = Value
  val goal, redLights, greenLights, roadEnd, roadStart, laneSwitch = Value
}