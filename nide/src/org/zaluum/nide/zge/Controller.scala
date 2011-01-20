package org.zaluum.nide.zge
import org.zaluum.nide.compiler.ScannedBoxClassPath
import org.zaluum.nide.model._
import scala.collection.mutable.{Buffer, Stack}

class Controller(val model: Model, val bcp :ScannedBoxClassPath) {
  private var viewModels = Buffer[ModelView]()
  def registerView(viewer:Viewer) = {
    val viewModel = new ModelView(viewer,model,bcp)
    viewModels += viewModel
    viewModel.update()
    viewModel
  }
  var undoStack = Stack[Command]()
  var redoStack = Stack[Command]()
  def updateViewers { viewModels foreach { _.update() } }
  var mark:Option[Command] = None
  def isDirty = undoStack.elems.headOption != mark 
  def markSaved() { mark = undoStack.elems.headOption}
  var block :Option[()=>Unit] = None
  def onExecute(block : => Unit) {this.block = Some({block _})}
  def exec(c:Command) {
    c.act()
    undoStack.push(c)
    redoStack.clear
    updateViewers
    block.foreach { _() }
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