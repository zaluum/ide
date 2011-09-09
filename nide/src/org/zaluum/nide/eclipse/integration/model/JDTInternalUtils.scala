package org.zaluum.nide.eclipse.integration.model
import org.eclipse.jdt.internal.compiler.lookup.InvocationSite
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding

object JDTInternalUtils {
  def aToString(aa: Array[Array[Char]]): String = aa.map { new String(_) }.mkString(".")
  def stringToA(str: String): Array[Array[Char]] = str.split('.').map { _.toCharArray }
}

