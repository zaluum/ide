package org.zaluum.nide.zge

import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.TreeToClass
import org.zaluum.nide.compiler.MapTransformer
import org.zaluum.nide.compiler.PrettyPrinter
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.compiler.{ Tree, Analyzer, Transformer, FakeGlobalScope, Reporter }
import scala.collection.mutable.{ Buffer, Stack }

class Controller(private var nowTree: Tree, val global: EclipseBoxClasspath) {
  private var viewers = Buffer[Viewer]()
  def registerViewer(viewer: Viewer) {
    viewers += viewer
    viewer.refresh()
  }
  def unregisterViewer(viewer: Viewer) {
    viewers -= viewer
  }
  def updateViewers(map: Map[SelectionSubject, SelectionSubject]) {
    viewers foreach { v ⇒ v.remapSelection(map); v.refresh() }
  }
  def refreshTools() { viewers foreach { _.tool.refresh() } }
  def tree = nowTree
  val reporter = new Reporter()
  def compile() = {
    val scope = new FakeGlobalScope(global)
    nowTree = new Analyzer(reporter, tree, scope).compile()
  }
  type DMap = Map[SelectionSubject, SelectionSubject]
  case class Mutation(before: Tree, d: DMap, now: Tree)
  var undoStack = Stack[Mutation]()
  var redoStack = Stack[Mutation]()
  var mark: Option[Mutation] = None
  def isDirty = undoStack.elems.headOption != mark
  def markSaved() { mark = undoStack.elems.headOption }
  def exec(c: MapTransformer) {
    val before = nowTree
    nowTree = c(tree)
    undoStack.push(Mutation(before, c.map, nowTree))
    redoStack.clear
    update(c.map)
  }
  private def update(m: DMap) {
    compile()
    PrettyPrinter.print(nowTree, 0)
    updateViewers(m)
    notifyListeners
    refreshTools
  }
  def canUndo = !undoStack.isEmpty
  def canRedo = !redoStack.isEmpty
  def undo() {
    if (!undoStack.isEmpty) {
      val mutation = undoStack.pop
      nowTree = mutation.before
      redoStack.push(mutation)
      update(mutation.d map { _.swap })
    }
  }
  def redo() {
    if (!redoStack.isEmpty) {
      val mutation = redoStack.pop;
      undoStack.push(mutation)
      nowTree = mutation.now
      update(mutation.d)
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

