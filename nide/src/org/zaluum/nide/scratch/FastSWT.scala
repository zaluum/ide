package org.zaluum.nide.scratch

import org.zaluum.nide.zge.ExampleGUI
import org.zaluum.nide.zge.GUIViewer
import org.zaluum.nide.zge.GUIModel
import org.zaluum.nide.zge.AbstractViewer
//import org.zaluum.nide.compiler.SimpleScannedBoxClassPath
//import org.zaluum.nide.compiler.SimpleBoxClassPath
import org.zaluum.nide.zge.SWTScala
import org.zaluum.nide.zge.Viewer
import org.zaluum.nide.zge.Controller
import org.zaluum.nide.compiler.BoxClassPath
import org.eclipse.swt.widgets.Widget
import java.io.FileInputStream
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionAdapter
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.layout.RowLayout
import java.io.FileOutputStream
import java.io.File
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.layout.FillLayout
import org.zaluum.nide.model._
import SWTScala._
/*
object FastSWT {

  def main(args: Array[String]) {
    new FastGUIViewer().run()
  }

}
class FastSWT {
  val display = new Display()
  val shell = new Shell(display)
  shell.setBounds(10, 10, 500, 500)
  shell.setLayout(new FillLayout)

  shell.setLayout(new FillLayout(SWT.VERTICAL))
  val buttons = new Composite(shell, SWT.NULL)
  buttons.setLayout(new RowLayout())

  def run() {
    shell.open()
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch()) display.sleep();
    }
    display.dispose();
  }
}
class FastGUIViewer extends FastSWT {
  var viewer : GUIViewer = null
  
  def createViewer(model: Model) {
    val bcp = new SimpleScannedBoxClassPath(new File("."), currentThread.getContextClassLoader)
    val controller = new Controller(new Model,bcp)
    if (viewer != null)
      viewer.dispose()
    viewer = new GUIViewer(shell, controller)
    viewer.canvas.setBounds(10, 10, 450, 450)
    shell.pack
  }
  val buttonFromCode = new Button(buttons, SWT.PUSH)
  buttonFromCode.setText("ModelFromCode")
  addReaction(buttonFromCode) {
    //createViewer(ExampleGUI.simpleModel)
  }
}
class FastViewer extends FastSWT {
  var viewer: Viewer = null
  val buttonLoad = new Button(buttons, SWT.PUSH)
  val bcp = new SimpleScannedBoxClassPath(new File("."), currentThread.getContextClassLoader)
  def createViewer(model: Model) {
    val controller = new Controller(model, bcp)
    if (viewer != null)
      viewer.dispose()
    viewer = new Viewer(shell, controller)
    viewer.canvas.setBounds(10, 10, 450, 450)
    shell.pack
  }
  buttonLoad.setText("Load")
  addReaction(buttonLoad) {
    val f = new FileInputStream("testOutput.zaluum")
    try {
      val model = ProtoModel.read(f, "org.zaluum.Test")
      createViewer(model)
    } finally { f.close() }
  }
  val buttonSave = new Button(buttons, SWT.PUSH)
  buttonSave.setText("Save")
  addReaction(buttonSave) {
    if (viewer != null) {
      val f = new FileOutputStream("testOutput.zaluum")
      try {
        ProtoModel.writeTo(viewer.controller.model, f)
      } finally (f.close())
    }
  }
  val buttonFromCode = new Button(buttons, SWT.PUSH)
  buttonFromCode.setText("ModelFromCode")
  addReaction(buttonFromCode) {
    createViewer(Example.sumsumModel)
  }
  val buttonGenerate = new Button(buttons, SWT.PUSH)
  buttonGenerate.setText("Generate")
  addReaction(buttonGenerate) {
    if (viewer != null) {
      println("generating...")
      val f = new FileOutputStream("Generated.java")
      val out = new java.io.PrintWriter(f);
      //new Generator().generate(viewer.controller.model,new CodeWriter(out))
      out.close
    }
  }
}
*/