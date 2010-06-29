package org.zaluum.ide.debug;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.statushandlers.StatusManager;

class ValueSelectDialog(shell:Shell, stype: String) extends TitleAreaDialog(shell) {
  class ValueWidget(parent:Composite) extends Composite(parent,SWT.NONE){
    setLayout(new FillLayout())
    val combo = stype match {
      case "enum" => 
        val combo = new Combo(this, SWT.DROP_DOWN | SWT.READ_ONLY);
        val vwr = new ComboViewer(combo);
        vwr.setContentProvider(ArrayContentProvider.getInstance());
        vwr.setLabelProvider(new LabelProvider());
        //vwr.setInput(((EnumCType) cType).getMembers().toArray());
        combo.select(0);
        combo
      case _ => new Combo(this, SWT.DROP_DOWN);
    }
    def getResult:Any = stype match {
        case "enum" => combo.getText()
        case _ => Integer.parseInt(combo.getText)
    }
  }
  var vw : ValueWidget = _
  var result : Any = _
  override protected def createDialogArea(p : Composite)  = {
		val c = super.createDialogArea(p).asInstanceOf[Composite]
		setTitle("Value selection")
	  setMessage("Select a value to push")
		val gd = new GridData(SWT.LEFT, SWT.TOP, true, true)
		vw = new ValueWidget(c)
		vw.setLayoutData(gd)
		c
	}

	override protected def okPressed {
		result = vw.getResult;
		super.okPressed();
	}
	
}