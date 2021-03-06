package org.zaluum.nide.zge.dialogs

import scala.collection.JavaConversions.asScalaBuffer
import org.eclipse.jface.dialogs.Dialog
import org.eclipse.jface.viewers.ArrayContentProvider
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.jface.viewers.ComboViewer
import org.eclipse.jface.viewers.ICellModifier
import org.eclipse.jface.viewers.ISelectionChangedListener
import org.eclipse.jface.viewers.IStructuredContentProvider
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.ITableLabelProvider
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.SelectionChangedEvent
import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.jface.viewers.TableViewer
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Table
import org.eclipse.swt.widgets.TableColumn
import org.eclipse.swt.widgets.TableItem
import org.eclipse.swt.SWT
import org.eclipse.ui.ISharedImages
import org.eclipse.ui.PlatformUI
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.ParamDecl
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.utils.SWTScala.addKeyReleasedReaction
import org.zaluum.nide.utils.SWTScala.newLabel
import org.zaluum.nide.utils.SWTScala.newMenuItem
import org.zaluum.nide.utils.SWTScala.newPopupMenu
import org.zaluum.nide.zge.Viewer
import net.miginfocom.swt.MigLayout
import org.zaluum.nide.compiler.Name
import org.eclipse.swt.widgets.Shell
import org.zaluum.nide.zge.Controller
import org.zaluum.nide.compiler.ClassJavaType
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.zaluum.nide.compiler.BoxExprType
import org.zaluum.nide.compiler.Param

class ConstructorSelectDialog(
    jproject: JavaProject,
    shell: Shell,
    vs: ValSymbol) extends Dialog(shell) {

  case class TableEntry(var sym: Option[(String, TypeBinding)], var value: String)
  class Loader(loader: ⇒ Iterable[AnyRef]) extends IStructuredContentProvider {
    def dispose() {}
    def getElements(inputElement: AnyRef): Array[AnyRef] = loader.toArray
    def inputChanged(viewer: org.eclipse.jface.viewers.Viewer, oldInput: Any, newInput: Any) {}
  }

  var combo: ComboViewer = _
  var tableContents = List[TableEntry]()
  val initSignature = vs.getStr(BoxExprType.constructorTypesDecl).getOrElse("")
  val initParams = vs.getList(BoxExprType.constructorParamsDecl).getOrElse(List())
  var result: Option[EditTransformer] = None

  def comboValue = {
    val sel = combo.getSelection.asInstanceOf[IStructuredSelection]
    if (sel.isEmpty) None
    else Some(sel.getFirstElement.asInstanceOf[MethodBinding])
  }
  def v = vs.decl
  override protected def okPressed() {
    val signature = comboValue.map(_.signature().mkString).getOrElse("")
    val params = tableContents.map(_.value)
    if (signature != initSignature || params != initParams) {
      result = Some(v.addOrReplaceParams(List(
        Param(BoxExprType.constructorParamsDecl.fqName, params),
        Param(BoxExprType.constructorTypesDecl.fqName, signature))))
    }
    super.okPressed()
  }
  def createTable(parent: Composite) = {
    val table = new Table(parent, SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL |
      SWT.FULL_SELECTION | SWT.HIDE_SELECTION)
    val nameCol = new TableColumn(table, SWT.RIGHT)
    table.setLinesVisible(true);
    table.setHeaderVisible(true);
      def newColumn(str: String, w: Int) {
        val col = new TableColumn(table, SWT.CENTER, 0);
        col.setText(str);
        col.setWidth(w);
      }
    newColumn("Type", 150)
    newColumn("Value", 150)
    newColumn("Name", 150)
    table
  }
  override def createDialogArea(parent: Composite): Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup, SWT.NONE)
    //applyDialogFont(c);
    c.setLayout(new MigLayout)
    newLabel(c, "Select the constructor to be used")
    combo = new ComboViewer(c)
    combo.setContentProvider(ArrayContentProvider.getInstance)
    val tpe = vs.classinfo.asInstanceOf[ClassJavaType]
    combo.setInput(tpe.allConstructors(tpe.scope).toArray)
    combo.getControl.setLayoutData("span,wrap")
    val constructor = vs match {
      case vs: ValSymbol ⇒ vs.constructor
      case _             ⇒ None
    }
    constructor foreach { cons ⇒
      combo.setSelection(new StructuredSelection(cons));
    }
    // TABLE
    tableContents = createTableValue(constructor, vs.getList(BoxExprType.constructorParamsDecl).getOrElse(List()))
    val table = createTable(c)
    table.setLayoutData("span,  height 200")
    val tableViewer = new TableViewer(table)
    val columnNames = Array("Name", "Type", "Value")
    tableViewer.setColumnProperties(columnNames);
    tableViewer.setUseHashlookup(true)
    tableViewer.setContentProvider(new Loader(tableContents))
    val editors: Array[CellEditor] = Array.ofDim(columnNames.length)
    editors(0) = null
    editors(1) = null
    editors(2) = new TextCellEditor(table)
    tableViewer.setCellEditors(editors)
    tableViewer.setCellModifier(new ICellModifier {
      def canModify(element: AnyRef, property: String) = property == "Value"
      def toTE(a: AnyRef) = a.asInstanceOf[TableItem].getData.asInstanceOf[TableEntry]
      def getValue(element: AnyRef, property: String) = element.asInstanceOf[TableEntry].value
      def modify(element: AnyRef, property: String, value: AnyRef) {
        toTE(element).value = value.asInstanceOf[String]
        tableViewer.refresh()
      }
    })
    tableViewer.setLabelProvider(new LabelProvider with ITableLabelProvider {
      def getColumnImage(element: AnyRef, columnIndex: Int): Image = null
      def getColumnText(element: AnyRef, columnIndex: Int) = {
        val te = element.asInstanceOf[TableEntry]
        columnIndex match {
          case 0 ⇒ te.sym map { _._1 } getOrElse { "?" }
          case 1 ⇒ te.sym map { _._2.readableName.mkString } getOrElse { "?" }
          case 2 ⇒ te.value
          case _ ⇒ null
        }
      }
    })
    tableViewer.setInput(this)
    // listeners
    combo.addPostSelectionChangedListener(new ISelectionChangedListener {
      def selectionChanged(event: SelectionChangedEvent) {
        tableContents = createTableValue(comboValue, tableContents.map { _.value } filter { _ != "" })
        tableViewer.refresh()
      }
    })
      // DELETE
      def delete() {
        val sel = tableViewer.getSelection.asInstanceOf[StructuredSelection]
        if (!sel.isEmpty) {
          import scala.collection.JavaConversions._
          val selected = sel.toList.toList.asInstanceOf[List[TableEntry]]
          val toDelete = selected filter { _.sym.isEmpty }
          if (!toDelete.isEmpty) {
            tableContents = tableContents filterNot (toDelete.contains(_))
            tableViewer.refresh()
          }
        }
      }
    newPopupMenu(getShell, table) { m ⇒
      newMenuItem(m, "Delete", Some(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ELCL_REMOVE))) {
        delete()
      }
    }
    addKeyReleasedReaction(table, SWT.DEL) { delete() }
    c
  }

  def createTableValue(c: Option[MethodBinding], values: List[String]) = {
    val withSymbol = c match {
      case Some(m) ⇒
        val mnames = new MethodWithNames(m, jproject)
        for (tuple ← mnames.paramsList) yield {
          TableEntry(Some(tuple), "")
        }
      case None ⇒ List()
    }
    val others = for (i ← withSymbol.length until values.length) yield new TableEntry(None, "")
    val table = withSymbol ++ others
    for ((v, t) ← values.zip(table)) t.value = v
    table
  }
  override def isResizable = true
}