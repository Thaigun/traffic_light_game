package graphical

import scala.swing._
import scala.io.Source
import mapLogic._
import javax.swing.UIManager

object MapGraph extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

  val game = new Game
  game.readFile
  var gameThread = new Thread(game)
  val gamePanel = new GamePanel(game)
  game.panel = gamePanel

  val startButton = new Button("Start Game") {
    action = new Action("Start Game") {
      def apply = {
        /*Start the game-related processing in a new thread*/
        try {
          if (!gameThread.isAlive()) {
            gameThread = new Thread(game)
            gameThread.start()
          }
        } catch {
          case _: Throwable =>
        }
      }
    }
  }
  val endButton = new Button("Give Up") {
    action = new Action("Give Up") {
      def apply = game.resigned = true
    }
  }
  val buttons = new BoxPanel(Orientation.Horizontal) {
    contents += startButton
    contents += endButton
  }
  val contentHolder = new BoxPanel(Orientation.Vertical) {
    contents += buttons
    contents += gamePanel
  }

  val gameWindow = new MainFrame() {
    title = "Traffic light challenge!"

    contents = contentHolder

  }

  def top() = this.gameWindow
}