package trafficLogic

import mapLogic._

class CrossingLane(game: Game)(startX: Int, startY: Int)(endX: Int, endY:Int) extends Lane(game)(startX, startY)(endX, endY) {
  var combosWhereGreen: Array[Char] = Array()
  var isGreenLight = false
  
}