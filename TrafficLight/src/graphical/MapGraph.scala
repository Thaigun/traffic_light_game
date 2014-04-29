package graphical

import scala.swing._
import scala.io.Source
import mapLogic._
import javax.swing.UIManager

object MapGraph extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  
  val game = new Game
  game.gameFile = Source.fromFile("src/gamefile.txt")
  game.readFile
  
  val gameWindow = new MainFrame() {
    title = "Traffic light challenge!"
    maximize()
    val gamePanel = new GamePanel(game)
    contents = gamePanel
  }

  def top() = this.gameWindow
}