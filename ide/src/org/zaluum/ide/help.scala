package org.zaluum.ide
import org.eclipse.help._
class ContextProvider(val str : String) extends IContextProvider {
  def getContextChangeMask() : Int = 0

  def getContext(target: Object) : IContext = null

  def getSearchExpression(target:Object) : String  = ""
}