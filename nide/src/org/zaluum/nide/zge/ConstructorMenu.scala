package org.zaluum.nide.zge

import org.eclipse.swt.layout.GridLayout
import org.eclipse.jface.dialogs.Dialog
import org.zaluum.nide.compiler._
import org.eclipse.jface.viewers._
import org.eclipse.swt.SWT
import org.eclipse.swt.SWT.NONE
import org.eclipse.swt.graphics.Image
import net.miginfocom.swt.MigLayout
import org.eclipse.swt.widgets._

class ConstructorMenu(shell:Shell,vs:ValSymbol) extends Dialog(shell) {
/*    shell,"Constructor selection",null,"Select constructor",MessageDialog.QUESTION,
    Array("OK","Cancel"),1) {*/
  var selectedCons : Option[Constructor] = None
  var values = Map[Name,Text]() 
  override protected def buttonPressed(buttonId : Int) {
    println(values)
    super.buttonPressed(buttonId);
  }
  def createTable(parent:Composite) = {
    val table = new Table(parent, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | 
          SWT.FULL_SELECTION | SWT.HIDE_SELECTION)
    val nameCol = new TableColumn(table,SWT.RIGHT)
    table.setLinesVisible(true);
    table.setHeaderVisible(true);
    def newColumn(str:String,w:Int) { 
      val col = new TableColumn(table, SWT.CENTER, 0);   
      col.setText(str);
      col.setWidth(w);
    }
    newColumn("Name",50)
    newColumn("Type",60)
    newColumn("Value",100)
    table
  }
  override def createDialogArea(parent:Composite) :Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup,NONE)
    //applyDialogFont(c);
    c.setLayout(new MigLayout)
    val l = new Label(c,NONE)
    l.setText("Select the constructor to be used")
    val combo = new ComboViewer(c)
    combo.setContentProvider(ArrayContentProvider.getInstance)
    val tpe = vs.tpe.asInstanceOf[BoxTypeSymbol]
    combo.setInput(tpe.constructors.toArray)
    combo.getControl.setLayoutData("span")
    val table = createTable(c)
    table.setLayoutData("span")
    val tableViewer = new TableViewer(table)
    val columnNames = Array("Name","Type","Value")
    tableViewer.setColumnProperties(columnNames);
    tableViewer.setUseHashlookup(true)
    tableViewer.setContentProvider(ArrayContentProvider.getInstance)
    val editors:Array[CellEditor] = Array.ofDim(columnNames.length)
    editors(0) = null
    editors(1) = null
    editors(2) = new TextCellEditor(table)
    tableViewer.setCellEditors(editors)
    tableViewer.setCellModifier( new ICellModifier {
        def canModify(element:AnyRef, property:String) = property=="Value"
        def getValue(element : AnyRef, property: String) = "hola"
        def modify(element:AnyRef, property:String, value:AnyRef) {}
        })
    tableViewer.setLabelProvider(new  LabelProvider with ITableLabelProvider{
      def getColumnImage(element:AnyRef, columnIndex : Int):Image = null
      def getColumnText(element : AnyRef, columnIndex : Int) = {
        val p = element.asInstanceOf[ParamSymbol]
        columnIndex match {
          case 0 => p.name.str
          case 1 => p.tpe.name.str
          case 2 => ""
          case _ => null
        }
      }
    })
    tableViewer.setInput(tpe.constructors.head.params.toArray)
    sup
  }
  override def isResizable = true
}