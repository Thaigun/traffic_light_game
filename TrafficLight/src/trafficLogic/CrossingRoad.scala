package trafficLogic

import mapLogic._

class CrossingRoad(game: Game) extends Road(game) {
  var combosWhereGreen: Array[Char] = Array()
  var isGreenLight = false
  
}