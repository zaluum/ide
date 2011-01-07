package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.Ellipse
import scala.collection.mutable.Stack
import scala.collection.mutable.Buffer
import org.zaluum.nide.model._ 

class Controller(val model: Model) {
  private var viewModels = Buffer[ModelView]()
  def registerView(viewer:Viewer) = {
    val viewModel = new ModelView(viewer,model)
    viewModels += viewModel
    viewModel.update()
    viewModel
  }
  var undoStack = Stack[Command]()
  var redoStack = Stack[Command]()
  def updateViewers { viewModels foreach { _.update() } }
  def exec(c:Command) {
    c.act()
    undoStack.push(c)
    redoStack.clear
    updateViewers
  }
  def undo(){
    if (!undoStack.isEmpty){
      val c = undoStack.pop
      redoStack.push(c)
      c.undo()
      updateViewers
    }
  }
  def redo(){
    if(!redoStack.isEmpty) {
      val c = redoStack.pop
      undoStack.push(c)
      updateViewers
    }
  }
}