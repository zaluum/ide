package org.zaluum.nide.zge

import org.zaluum.nide.newcompiler.Reporter
import org.zaluum.nide.newcompiler.FakeGlobalScope
import org.zaluum.nide.newcompiler.LocalScope
import org.zaluum.nide.newcompiler.Transformer
import org.zaluum.nide.newcompiler.{Tree,Analyzer}
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.model._
import scala.collection.mutable.{ Buffer, Stack }

trait TreeCommand {
  def execute(tree:Tree) : Tree
  def canExecute : Boolean
}
object TreeCommand {
  def apply(t:Transformer) = new TreeCommand{
    def execute(tree:Tree) = t(tree)
    def canExecute = true
  }
}
class Controller(private var treep: Tree,val global:EclipseBoxClasspath) {
  private var viewModels = Buffer[AbstractModelView]()
  def registerView(modelView: AbstractModelView) {
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
    val scope = new FakeGlobalScope(global)
    treep = new Analyzer(reporter,tree,scope).compile()
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
      treep = c.execute(tree)
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


