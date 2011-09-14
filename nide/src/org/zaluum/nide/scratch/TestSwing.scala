package org.zaluum.nide.scratch
import javax.swing.JFrame
import org.zaluum.widget.BooleanIndicator
import java.awt.Dimension
import org.zaluum.widget.ImageReader
import org.zaluum.nide.icons.Icons

object TestSwing {
  def main(args: Array[String]): Unit = {
    val frame = new JFrame()
    val b = new BooleanIndicator()
    val urltrue = classOf[Icons].getResource("buttonIfTrue.png")
    b.setFalseImage(ImageReader.readImageResource("org/zaluum/nide/icons/buttonIfFalse.png"))
    b.setPreferredSize(new Dimension(40, 40));
    frame.add(b)
    frame.setVisible(true);
  }
}
