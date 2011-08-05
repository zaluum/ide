package org.zaluum.nide.eclipse
import org.eclipse.help.IContext
import org.eclipse.help.IContextProvider
class ContextProvider(val str: String) extends IContextProvider {
  def getContextChangeMask(): Int = 0

  def getContext(target: Object): IContext = null

  def getSearchExpression(target: Object): String = ""
}