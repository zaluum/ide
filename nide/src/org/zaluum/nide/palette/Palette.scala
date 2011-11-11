package org.zaluum.nide.palette
import scala.Array.canBuildFrom
import scala.collection.breakOut
import scala.collection.Map
import org.eclipse.jdt.core.IType
import org.zaluum.annotation.Apply
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.BoxExprType
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.Expressions
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.Param
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.PortDir
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.compiler.Template
import org.zaluum.nide.compiler.ValDef
import org.eclipse.jdt.core.Flags
import org.eclipse.jdt.core.IMethod
import org.eclipse.jdt.core.JavaCore

class Palette(val root: Pkg, val map: Map[String, List[PaletteEntry]])

case class Pkg(val name: String, val child: List[Pkg], val entries: List[PaletteEntry])

case class PaletteEntry(
    name: String,
    pkg: String,
    tpe: String,
    parameters: Map[String, List[String]]) {
  def className: Name = parameters.get("#Class").flatMap(_.headOption map (Name(_))).getOrElse(Name(tpe))

  def toValDef(b: Block, dst: Point, size: Option[Dimension], template: Option[Template], overrideParams: List[Param]) = {
    val rawparams: List[Param] = parameters.map { case (k, l) ⇒ Param(Name(k), l) }(breakOut)
    val params = rawparams.filterNot(p ⇒ overrideParams.exists(_.key == p.key)) ++ overrideParams
    val newname = Name(b.sym.freshName(name))
    ValDef(newname, Name(tpe), dst, size, params, None, None, template)
  }
}

object PaletteEntry {
  def box(className: Name) = {
    PaletteEntry(className.classNameWithoutPackage.str, className.packageProxy,
      BoxExprType.fqName.str, Map("#Class" -> List(className.str)))
  }

  def expression(className: Name) = {
    PaletteEntry(className.classNameWithoutPackage.str, className.packageProxy,
      className.str, Map())
  }
  val portsPkg = "<ports>"
  private def portToProxy(port: PortDir) = PaletteEntry(
    port.str,
    portsPkg,
    "port",
    Map())
  val InEntry = portToProxy(In)
  val OutEntry = portToProxy(Out)
  val ShiftEntry = portToProxy(Shift)
  val portsEntries = List(InEntry, OutEntry, ShiftEntry)

  def load(t: IType): Array[PaletteEntry] =
    try {
      val classname = Name(t.getFullyQualifiedName)
      if (Expressions.find(classname).isDefined) {
        Array(PaletteEntry.expression(classname))
      } else {
        val methods = t.getMethods()
        val applyMethods = for (
          m ← methods;
          if (m.getAnnotations.exists(a ⇒
            a.getElementName == classOf[Apply].getName ||
              a.getElementName == "Apply")) // TODO model is source only? not resolved.
        ) yield { m }
        if (applyMethods.size == 0)
          Array(PaletteEntry.box(classname))
        else {
          Array(PaletteEntry.box(classname))
          /*applyMethods map { m ⇒
            if (Flags.isStatic(m.getFlags))
              FIXME PaletteEntry.static(className, m)
            else
              FIXME PaletteEntry.box(m)
          }*/
        }
      }
    } catch { case ex: Exception ⇒ Array() }
}
