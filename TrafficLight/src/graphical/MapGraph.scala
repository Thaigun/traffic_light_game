package graphical

import scala.swing._
import javax.swing.UIManager

object MapGraph extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

  val gameWindow = new MainFrame() {
    title = "Traffic light challenge!"
    maximize()
  }

  def top() = this.gameWindow
}