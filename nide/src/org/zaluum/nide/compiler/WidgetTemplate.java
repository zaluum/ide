package org.zaluum.nide.compiler;

import javax.swing.JComponent;
import javax.swing.JPanel;

public class WidgetTemplate {
  public JComponent widget;
  public WidgetTemplate2 b;
  public WidgetTemplate() {
    widget = new JPanel(null);
    widget.setSize(456, 789); 
    b.widget.setBounds(12,34,56,78); 
    widget.add(b.widget);
  }
}
