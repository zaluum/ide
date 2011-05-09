package org.zaluum.nide.zge

import org.zaluum.nide.Subject
import org.zaluum.nide.Observer
import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.nide.compiler._
import scala.collection.mutable.{ Buffer, Stack }
import org.zaluum.nide.Utils.inSWT
class Controller(private var nowTree: Tree, val global: ZaluumProject) {
  private var viewers = Buffer[Viewer]()
  def registerViewer(viewer: Viewer) {
    viewers += viewer
    viewer.refresh()
  }
  def unregisterViewer(viewer: Viewer) {
    viewers -= viewer
  }
  def updateViewers(map: Map[SelectionSubject, SelectionSubject]) {
    viewers foreach { v ⇒ 
      inSWT{
        v.remapSelection(map); 
        v.refresh(); 
      }(v.display)
    }
  }
  val observer = new Observer {
    def receiveUpdate(subject: Subject) {
      recompile
    }
  }
  global.addObserver(observer)
  def dispose() { global.removeObserver(observer) }
  def refreshTools() { viewers foreach { _.tool.refresh() } }
  def blink(s:SelectionSubject, fromViewer:Viewer) {
    viewers filterNot {_==fromViewer} foreach { _.blink(s) }
  }
  def tree = nowTree
  val reporter = new Reporter()
  def compile() = {
    val scope = new FakeGlobalScope(global)
    nowTree = new Analyzer(reporter, tree.asInstanceOf[BoxDef], scope).compile()
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
  def usedTypes : Seq[Type] = {
    val b = scala.collection.mutable.Set[Type]()
    val t = new Traverser(global) {
      override def traverse(tree:Tree) = {
        tree match {
          case t:ValDef if (t.tpe!=NoSymbol)=> b+=t.tpe 
          case _ =>
        }
        super.traverse(tree)
      }
    }
    t.traverse(nowTree)
    b.toSeq
  }
  def noChangeMap = {
    var map:DMap = Map()
    new Traverser(global) {
      override def traverse(tree:Tree) = {
        map += (tree->tree)
      }
    }
    map
  }
  def recompile {
    update(noChangeMap)
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

