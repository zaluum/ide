package org.zaluum.nide.eclipse

import org.eclipse.jdt.core.IJavaProject
import org.zaluum.nide.compiler.{Name,BoxTypeSymbol}
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.Flags
import org.eclipse.core.runtime.IProgressMonitor
import org.zaluum.annotation.Box
import scala.collection.mutable.WeakHashMap
import org.zaluum.nide.zge.ImageFactory
import org.eclipse.swt.widgets.Display
import org.zaluum.nide.compiler.Expressions

case class BoxTypeProxy(name:Name, template:Boolean) {
  def split = name.str.splitAt(name.str.lastIndexOf("."))
  def pkgName = split._1 
  def simpleName = split._2.drop(1)
}

object ZaluumProjectManager {
  private val m = WeakHashMap[IJavaProject,ZaluumProject]()
  def getZaluumProject(jProject: IJavaProject) = {
    m.getOrElseUpdate(jProject, new ZaluumProject(jProject))
  }
}
class ZaluumProject private[eclipse] (val jProject: IJavaProject) extends GlobalClassPath{
  lazy val imageFactory = new ImageFactory(this)
  def getBoxSymbol(name:Name) : Option[BoxTypeProxy] = {
    Option(jProject.findType(name.str)) flatMap { typeToProxy(_) }
  }
  def index(monitor:IProgressMonitor=null) : Seq[BoxTypeProxy] = {
    import SearchUtils._
    val l = search(patternAnnotation(classOf[Box].getName),projectScope(jProject),monitor)
    l flatMap (typeToProxy(_))
  }
  def typeToProxy(t:IType) : Option[BoxTypeProxy] = {
    val name = Name(t.getFullyQualifiedName)
      Some(
          BoxTypeProxy(
              name,
              Expressions.isTemplateExpression(name)))
  }
}