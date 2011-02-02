package org.zaluum.nide.zge

import org.zaluum.nide.compiler.Reporter
import org.zaluum.nide.newcompiler.{Tree,Analyzer}
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.compiler.BoxClassPath
import org.zaluum.nide.compiler.ScannedBoxClassPath
import org.zaluum.nide.model._
import scala.collection.mutable.{ Buffer, Stack }
trait TreeCommand {
  def execute() : Tree
  def canExecute : Boolean
}
class Controller(private var treep: Tree,val global:EclipseBoxClasspath) {
  private var viewModels = Buffer[AbstractModelView]()
  def registerView(modelView: AbstractModelView) {
    //val viewModel = new ModelView(viewer, model, bcp)
    viewModels += modelView
    modelView.update()
  }
  def unregisterView(modelView : AbstractModelView) {
    viewModels -= modelView
  }
  def updateViewers { viewModels foreach { _.update() } }
  def abortTools() { viewModels foreach { _.viewer.tool.state.abort() } }
  def tree = treep
  val reporter = new Reporter()
  def compile() = {
    treep = new Analyzer(reporter,tree,global).compile()
  }
  var undoStack = Stack[Tree]()
  var redoStack = Stack[Tree]()
  var mark: Option[Tree] = None
  def isDirty = undoStack.elems.headOption != mark
  def markSaved() { mark = undoStack.elems.headOption }
  def exec(c: TreeCommand) {
    if (c.canExecute) {
      abortTools()
      undoStack.push(tree)
      treep = c.execute()
      compile()
      redoStack.clear
      updateViewers
      notifyListeners
    }
  }
  def canUndo = !undoStack.isEmpty
  def canRedo = !redoStack.isEmpty
  def undo() {
    if (!undoStack.isEmpty) {
      abortTools()
      redoStack.push(tree)
      treep = undoStack.pop
      compile()
      updateViewers
      notifyListeners
    }
  }
  def redo() {
    if (!redoStack.isEmpty) {
      abortTools()
      undoStack.push(tree)
      treep = redoStack.pop
      compile()
      updateViewers
      notifyListeners
    }
  }
  var listeners = Set[() ⇒ Unit]()
  def addListener(action: () ⇒ Unit) {
    listeners += action
  }
  def removeListener(action: () ⇒ Unit) {
    listeners -= action
  }
  def notifyListeners() {
    listeners foreach { _() }
  }
  compile()
}


