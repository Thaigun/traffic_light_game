package graphical

import mapLogic._
import java.awt.image.ImageObserver
import java.awt.Font
import trafficLogic._
import scala.collection.mutable.Buffer
import scala.swing._
import java.awt.Color
import scala.swing.Dialog._
import java.awt.BasicStroke
import scala.swing.event._

class GamePanel(game: Game) extends Panel with ImageObserver {
  this.requestFocusInWindow
  this.preferredSize = new Dimension(game.size._1, game.size._2)
  val roads: Array[RoadGraph] = game.roads.map(_.graphic)
  val crossings: Array[CrossingGraph] = game.crossings.map(_.graphic)
  val cl: Array[CrossingLane] = game.crossings.foldLeft(Array[CrossingLane]())((array, crossing) => array ++ crossing.lanes)
  val crossingLanes = cl.map(_.graphic)
  val buttons = game.buttons

  this.background = Color.WHITE

  def highScore = {
    if (game.over && !game.resigned) {
      val name = Dialog.showInput(parent = this, message = "Please enter your name for highscore", title = "Enter name", messageType = Message.Question, initial = "")
      name match {
        case Some(s) => game.fileReader.saveScore(new Score(game.goal, game.elapsed, name.get))
        case None => game.fileReader.saveScore(new Score(game.goal, game.elapsed, "Anonymous"))
      }
      showScores
    }
  }
  
  def showScores = Dialog.showMessage(this, game.fileReader.readScores, "Scoreboard", Message.Plain)

  def imageUpdate(img: java.awt.Image, infoflags: Int, x: Int, y: Int, width: Int, height: Int) = {
    if ((infoflags & ImageObserver.ALLBITS) != 0) {
      repaint();
      false; // Return false to say we don't need further notification. 
    }
    true; // Image has not finished loading, need further notification. 
  }
  
  override def paintComponent(g: Graphics2D) = {
    val carGraphs = game.cars.map(_.graphic)

    g.setBackground(Color.white)
    g.clearRect(0, 0, this.size.getWidth.toInt, this.size.getHeight().toInt)

    for (road <- roads) {
      g.setColor(Constants.roadColor)
      g.fill(road.outline)
      g.fill(road.sector)
      g.fill(road.endSect)
      g.setColor(Constants.trafLineColor)
      g.setStroke(new BasicStroke(1.0f,
        BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER,
        10.0f, Array(10.0.toFloat), 0.0f))
      road.trafLines.foreach(g.draw(_))
      if (road.road.hasNextCross) road.endSectLines.foreach(g.draw(_))
      if (road.road.hasPrevRoad || road.road.hasPrevCross) road.sectorLines.foreach(g.draw(_))

    }

    g.setStroke(new BasicStroke())
    for (crossing <- crossings) {
      g.setColor(Constants.roadColor)
      g.fill(crossing.outLine)
      g.setColor(Constants.trafLineColor)
      g.draw(crossing.outLine)
    }

    for (lane <- crossingLanes) {
      g.setColor(lane.color)
      g.draw(lane.arrow)
    }

    for (car <- carGraphs) {
      g.setColor(car.color)
      g.fill(car.outline)
    }

    for (button <- buttons) {
      g.drawImage(button.getIcon.getImage(), button.point.getX().toInt, button.point.getY().toInt, this)
    }

    g.setColor(Color.red)
    g.setFont(new Font(Font.SANS_SERIF, 0, 15))
    g.drawString("Score: " + game.score, 10, 20)
    g.drawString("Goal:  " + game.goal, 10, 40)
    if (game.started != 0) {
      g.drawString("Time:  " + game.elapsed, 10, 60)
    }

    if (game.resigned) {
      g.setFont(new Font(Font.SANS_SERIF, 0, 40))
      g.drawString("Start again by clicking the Start-button!", 140, 60)
    }

  }

}