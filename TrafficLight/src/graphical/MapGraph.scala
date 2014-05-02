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
  val gamePanel = new GamePanel(game)
  game.panel = gamePanel
  
  val gameWindow = new MainFrame() {
    title = "Traffic light challenge!"
    maximize()
    contents = gamePanel
    
  }
  
  val gameThread = new Thread(game)  
  gameThread.start()

  def top() = this.gameWindow
}