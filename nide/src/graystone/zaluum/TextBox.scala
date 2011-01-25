package graystone.zaluum

import javax.swing.JButton
import javax.swing.JComponent
import org.zaluum.nide.java.Widget
import org.zaluum.nide.java.In
import org.zaluum.nide.java.Box

@Box
@Widget("graystone.zaluum.TextBoxWidget")
class TextBox {
  @In(x=0,y=24) var in = 0.0
  def act(){}
}

class TextBoxWidget extends JButton {
  setText("hola")
}