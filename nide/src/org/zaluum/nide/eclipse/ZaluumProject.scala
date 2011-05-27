package org.zaluum.nide.eclipse
import org.eclipse.jdt.core.IJavaProject
import org.zaluum.nide.compiler.{Name,BoxTypeSymbol}
import org.zaluum.annotation.Box
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.Flags
import org.eclipse.core.runtime.IProgressMonitor

case class BoxTypeProxy(name:Name, abstractCl:Boolean)
class ZaluumProject(val jProject: IJavaProject) extends GlobalClassPath{
  def getBoxSymbol(name:Name) : Option[BoxTypeProxy] = {
    Option(jProject.findType(name.str)) flatMap { typeToProxy(_) }
  }
  def index(monitor:IProgressMonitor=null) : Seq[BoxTypeProxy] = {
    import SearchUtils._
    val l = search(patternAnnotation(classOf[Box].getName),jProject,monitor) { a => a }
    l flatMap (typeToProxy(_))
  }
  def typeToProxy(t:IType) : Option[BoxTypeProxy] = {
      Some(
          BoxTypeProxy(
              Name(t.getFullyQualifiedName),
              Flags.isAbstract(t.getFlags())))
  }
}