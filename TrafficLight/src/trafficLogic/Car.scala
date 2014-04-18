package trafficLogic

import mapLogic._
import graphical._

class Car(game: Game) {
  val length: Double = randomLength()
  val width: Double = widthForLength()
  val mass: Double = massForDimensions()

  def randomLength(): Double = {
    60.0 //Dummy value for now, will be random.
  }

  def widthForLength(): Double = {
    20.0 //Dummy value for now, will be random.
  }

  def massForDimensions(): Double = {
    1000 //Dummy value for now, will be random.
  }

  private var direction: Double = 0
  private var speed: Double = 0
  private var acceleration: Double = 0
  private var accDirection: Double = 0
  
  val graphic = new CarGraph()
  
}