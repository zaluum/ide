package org.zaluum.nide.zge

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


object FastSWT {
  def addReaction(b:Button, r : =>Unit) {
    b.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(e: SelectionEvent) {
        r
      }
    })
  }
  def main(args: Array[String]) {
    val display = new Display()
    val shell = new Shell(display)
    shell.setBounds(10, 10, 500, 500)
    shell.setLayout(new FillLayout)
    
    shell.setLayout(new FillLayout(SWT.VERTICAL))
    val buttons = new Composite(shell, SWT.NULL)
    buttons.setLayout(new RowLayout())
    val buttonLoad = new Button(buttons, SWT.PUSH)
    var viewer: Viewer = null
    def createViewer(model: Model) {
      val controller = new Controller(model)
      if (viewer != null)
        viewer.dispose()
      viewer = new Viewer(shell, controller)
      viewer.canvas.setBounds(10, 10, 450, 450)
      shell.pack
    }
    buttonLoad.setText("Load")
    addReaction(buttonLoad, {
      val f = new FileInputStream("testOutput.zaluum")
        try {
          val model = ProtoModel.read(f)
          createViewer(model)
        } finally { f.close() }
    })
    val buttonSave = new Button(buttons, SWT.PUSH)
    buttonSave.setText("Save")
    addReaction(buttonSave, {
        if (viewer == null) return
        val f = new FileOutputStream("testOutput.zaluum")
        try {
          ProtoModel.writeTo(viewer.controller.model,f)
        } finally (f.close())
    })
    val buttonFromCode = new Button(buttons, SWT.PUSH)
    buttonFromCode.setText("ModelFromCode")
    addReaction(buttonFromCode,{ 
      createViewer(Example.sumsumModel)
    }) 
    val buttonGenerate = new Button(buttons,SWT.PUSH)
    buttonGenerate.setText("Generate")
    addReaction(buttonGenerate, {
      if (viewer==null) return
      println("generating...")
      val f = new FileOutputStream("Generated.java")
      val out = new java.io.PrintWriter(f);
      //new Generator().generate(viewer.controller.model,new CodeWriter(out))
      out.close
    })
    //viewer.canvas.setPreferredSize(450,450)
    //shell.pack()
    shell.open()
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch()) display.sleep();
    }
    display.dispose();
  }

}