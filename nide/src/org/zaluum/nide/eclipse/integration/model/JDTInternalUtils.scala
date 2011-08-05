package org.zaluum.nide.eclipse.integration.model
import org.eclipse.jdt.internal.compiler.lookup.InvocationSite
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding

object JDTInternalUtils {
  def aToString(aa: Array[Array[Char]]): String = aa.map { new String(_) }.mkString(".")
  def stringToA(str: String): Array[Array[Char]] = str.split('.').map { _.toCharArray }
}

class FakeInvocationSite(val expectedType: TypeBinding) extends InvocationSite {
  def genericTypeArguments() = null
  def isSuperAccess() = false
  def isTypeAccess() = false
  def setActualReceiverType(receiverType: ReferenceBinding) {}
  def setDepth(depth: Int) {}
  def setFieldIndex(depth: Int) {}
  def sourceEnd() = 0
  def sourceStart() = 0
}
