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
  def abortTools() {    viewModels foreach { _.viewer.tool.state.abort()}}
  def exec(c:Command) {
    abortTools()
    c.act()
    undoStack.push(c)
    redoStack.clear
    updateViewers
    notifyListeners
  }
  def canUndo = !undoStack.isEmpty
  def canRedo = !redoStack.isEmpty
  
  def undo(){
    if (!undoStack.isEmpty){
      abortTools()
      val c = undoStack.pop
      redoStack.push(c)
      c.undo()
      updateViewers
      notifyListeners
    }
  }
  def redo(){
    if(!redoStack.isEmpty) {
      abortTools()
      val c = redoStack.pop
      undoStack.push(c)
      // update tools
      c.redo()
      updateViewers
      notifyListeners
    }
  }
  var listeners =  Set[()=>Unit]()
  def addListener(action: ()=>Unit) {
   listeners += action 
  }
  def removeListener(action: ()=>Unit) {
    listeners -= action
  }
  def notifyListeners () {
    listeners foreach { _.apply() }
  }
}
