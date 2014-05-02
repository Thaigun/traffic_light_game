package trafficLogic

import mapLogic._
import graphical._
import java.awt.geom.Point2D

class Car(game: Game) {
  val length: Double = randomLength()
  val width: Double = widthForLength()
  val mass: Double = massForDimensions()

  def randomLength(): Double = {
    1.0 * Constants.laneWidth //Dummy value for now, will be random.
  }

  def widthForLength(): Double = {
    0.5 * Constants.laneWidth //Dummy value for now, will be random.
  }

  def massForDimensions(): Double = {
    1000 //Dummy value for now, will be random.
  }
  
  var location: Point2D.Double = new Point2D.Double(0,0)
  private var direction: Double = 0
  private var speed: Double = 0
  private var acceleration: Double = 0
  private var accDirection: Double = 0
  
  val graphic = new CarGraph(this)
  
  def x = location.getX()
  def y = location.getY()
}