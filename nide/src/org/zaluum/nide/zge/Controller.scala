package org.zaluum.nide.zge

import org.zaluum.nide.Subject
import org.zaluum.nide.Observer
import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.nide.compiler._
import scala.collection.mutable.{ Buffer, Stack }
import org.zaluum.nide.Utils.inSWT
import org.eclipse.jdt.internal.core.CompilationUnit
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.AST
import org.zaluum.nide.eclipse.integration.model.ZaluumDomCompilationUnit
import org.eclipse.text.edits.TextEdit
import org.eclipse.text.edits.ReplaceEdit
import java.nio.charset.Charset
import org.eclipse.jdt.core.ICompilationUnit
class Controller(val cu: ICompilationUnit) {
  var nowTree : Tree = _
  cu.becomeWorkingCopy(null)
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
  def refreshTools() { viewers foreach { _.tool.refresh() } }
  def blink(s:SelectionSubject, fromViewer:Viewer) {
    viewers filterNot {_==fromViewer} foreach { _.blink(s) }
  }
  def tree = nowTree
  val reporter = new Reporter()
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
    replaceWorkingCopyContents()
    update(c.map)
  }
  def replaceWorkingCopyContents() {
    val p = Serializer.proto(nowTree.asInstanceOf[BoxDef]);
    val str =  new String(p.toByteArray,Charset.forName("ISO-8859-1"))
    cu.applyTextEdit(new ReplaceEdit(0,cu.getBuffer.getLength,str ),null)    
  }
  /*def usedTypes : Seq[Type] = {
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
  }*/
  def noChangeMap = {
    var map:DMap = Map()
    new Traverser(null) {
      override def traverse(tree:Tree) = {
        map += (tree->tree)
      }
    }
    map
  }
  def compile(){
    val parser = ASTParser.newParser(AST.JLS3)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setSource(cu)
    parser.setResolveBindings(true)
    parser.setIgnoreMethodBodies(true)
    val domcu = parser.createAST(null)
    domcu match {
      case z:ZaluumDomCompilationUnit =>
        nowTree = z.tree
      case _ => throw new Exception()
    }
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

