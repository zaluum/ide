package org.zaluum.nide.zge.dialogs

import java.util.Comparator
import scala.collection.JavaConversions
import org.eclipse.core.runtime.Status
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.lookup.Binding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jface.dialogs.DialogSettings
import org.eclipse.jface.dialogs.IDialogSettings
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.ClassJavaType
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.InvokeExprType
import org.zaluum.nide.compiler.Param
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.zaluum.nide.eclipse.integration.model.MethodUtils
import org.zaluum.nide.zge.Viewer
import org.zaluum.nide.compiler.StaticExprType
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog
import org.eclipse.jface.window.Window
import org.zaluum.nide.compiler.Name
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.ui.PlatformUI
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.search.IJavaSearchConstants
// XXX merge in a single menu with Field and Method
class StaticSelectDialog(viewer: Viewer, val vs: ValSymbol) {
  def work(className: String) {
    val tpe = vs.tpe.asInstanceOf[StaticExprType]
    val param = Param(tpe.typeName, className)
    val tr = vs.tdecl.addOrReplaceParam(param)
    viewer.controller.exec(tr)
  }
  
  def open() {
    // TODO share with portpopup
    val scope = SearchEngine.createJavaSearchScope(Array[IJavaElement](viewer.zproject.jProject))
    val o = new OpenTypeSelectionDialog(viewer.shell, false, PlatformUI.getWorkbench().getProgressService(), scope, IJavaSearchConstants.TYPE)
    if (o.open() == Window.OK) {
      val result = if (o.getResult == null) None else o.getResult.headOption
      result.foreach { r ⇒
        work(r.asInstanceOf[IType].getFullyQualifiedName())
      }
    }

  }
}
