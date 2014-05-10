package graphical

import scala.swing._
import scala.io.Source
import mapLogic._
import javax.swing.UIManager

import javax.sound._
import javax.sound.sampled._

object MapGraph extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

  //Code from http://stackoverflow.com/questions/10570345/java-getaudioinputstream-symbol-not-found
  //A background music can be set by adding a music.wav file in the src-folder 
  def playSound = {
    try {
      val audioInputStream = AudioSystem.getAudioInputStream(new java.io.File("src/music.wav"));
      val clip = AudioSystem.getClip();
      clip.open(audioInputStream);
      clip.start();
    } catch {
      case e: Throwable => {
        
      }
    }
  }
  playSound

  val game = new Game
  game.readFile
  var gameThread = new Thread(game)

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
  val scoreButton = new Button("Highscores") {
    action = new Action("Highscores") {
      def apply = game.panel.showScores
    }
  }
  val buttons = new BoxPanel(Orientation.Horizontal) {
    contents += startButton
    contents += endButton
    contents += scoreButton
  }
  val contentHolder = new BoxPanel(Orientation.Vertical) {
    contents += buttons
    contents += game.panel
  }

  val gameWindow = new MainFrame() {
    title = "Traffic light challenge!"

    contents = contentHolder

  }

  def top() = this.gameWindow
}