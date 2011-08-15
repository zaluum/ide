package org.zaluum.nide.zge.dialogs

import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog
import org.eclipse.jface.fieldassist.AutoCompleteField
import org.eclipse.jface.fieldassist.TextContentAdapter
import org.eclipse.jface.window.Window
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.events.KeyEvent
import org.eclipse.swt.events.KeyListener
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.SWT
import org.eclipse.ui.PlatformUI
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.PortDef
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.primitives
import org.zaluum.nide.zge.SWTScala.addReaction
import org.zaluum.nide.zge.ScrollPopup
import org.zaluum.nide.zge.TreeViewer
import net.miginfocom.swt.MigLayout
import org.zaluum.nide.zge.Popup

class PortDeclPopup(
    viewer: TreeViewer,
    portDef: PortDef) extends Popup(viewer.shell) {
  def name = "Port " + portDef.name.str
  def populate(content: Composite) {
    content.setLayout(new MigLayout( 
        "",
        "[][grow][]",
        "[]"))
    val lbl = new Label(content, SWT.NONE)
    lbl.setText("Type")
    val txt = new Text(content, SWT.BORDER)
    txt.setLayoutData("width 125::, growx")
    txt.setText(portDef.typeName.str)
    val proposals = primitives.allTypes map { _.name.str } sorted
    val complete = new AutoCompleteField(txt, new TextContentAdapter(), proposals.toArray)
    val srchbtn = new Button(content, SWT.PUSH)
    srchbtn.setText("Search...")
    srchbtn.setLayoutData("align right, wrap")
    txt.selectAll()
    txt.setFocus
      def work(now: String) {
        if (now != null) {
          val name = Name(now)
          viewer.controller.exec(new EditTransformer {
            val trans: PartialFunction[Tree, Tree] = {
              case p: PortDef if (p == portDef) ⇒ p.copy(typeName = name)
            }
          })
        }
        hide
      }
    txt.addKeyListener(new KeyListener() {
      def keyPressed(e: KeyEvent) {
        if (e.keyCode == SWT.CR) work(txt.getText())
      }
      def keyReleased(e: KeyEvent) {}
    })
      def search {
        // XXX filter non visible types
        val scope = SearchEngine.createJavaSearchScope(Array[IJavaElement](viewer.zproject.jProject))
        val o = new OpenTypeSelectionDialog(viewer.shell, false, PlatformUI.getWorkbench().getProgressService(), scope, IJavaSearchConstants.TYPE)
        if (o.open() == Window.OK) {
          val result = if (o.getResult == null) None else o.getResult.headOption
          result.foreach { r ⇒
            work(r.asInstanceOf[IType].getFullyQualifiedName())
          }
        }
      }
    addReaction(txt) { work(txt.getText) }
    addReaction(srchbtn) { search }
  }
}