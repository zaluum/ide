package org.zaluum.nide.zge
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.model._
import org.zaluum.nide.newcompiler.{ Tree, Analyzer, Transformer, FakeGlobalScope, Reporter }
import scala.collection.mutable.{ Buffer, Stack }

trait TreeCommand {
  def execute(tree: Tree): Tree
  def canExecute: Boolean
}
object TreeCommand {
  def apply(t: Transformer) = new TreeCommand {
    def execute(tree: Tree) = t(tree)
    def canExecute = true
  }
}
class Controller(private var treep: Tree, val global: EclipseBoxClasspath) {
  private var viewers = Buffer[Viewer]()
  def registerViewer(viewer: Viewer) {
    println("registering viewer " + viewer)
    viewers += viewer
    viewer.update()
  }
  def unregisterViewer(viewer: Viewer) {
    viewers -= viewer
  }
  def updateViewers { viewers foreach { _.update() } }
  def refreshTools() { viewers foreach { _.tool.refresh() } }
  def tree = treep
  val reporter = new Reporter()
  def compile() = {
    val scope = new FakeGlobalScope(global)
    treep = new Analyzer(reporter, tree, scope).compile()
  }
  var undoStack = Stack[Tree]()
  var redoStack = Stack[Tree]()
  var mark: Option[Tree] = None
  def isDirty = undoStack.elems.headOption != mark
  def markSaved() { mark = undoStack.elems.headOption }
  def exec(c: TreeCommand) {
    if (c.canExecute) {
      undoStack.push(tree)
      treep = c.execute(tree)
      compile()
      redoStack.clear
      updateViewers
      notifyListeners
      refreshTools
    }
  }
  def canUndo = !undoStack.isEmpty
  def canRedo = !redoStack.isEmpty
  def undo() {
    if (!undoStack.isEmpty) {
      redoStack.push(tree)
      treep = undoStack.pop
      compile()
      updateViewers
      notifyListeners
      refreshTools
    }
  }
  def redo() {
    if (!redoStack.isEmpty) {
      undoStack.push(tree)
      treep = redoStack.pop
      compile()
      updateViewers
      notifyListeners
      refreshTools
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

