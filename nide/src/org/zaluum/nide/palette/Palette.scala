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
import org.zaluum.nide.compiler.InvokeStaticExprType
import org.zaluum.nide.compiler.Signatures
import org.zaluum.annotation.Box

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
  def static(className: Name, m: IMethod) = {
    PaletteEntry(className.classNameWithoutPackage.str + "." + m.getElementName, className.packageProxy,
      InvokeStaticExprType.fqName.str,
      Map("#Class" -> List(className.str),
        "#Method" -> List(Signatures.methodAndArity(m))))
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
  def loadStatic(t: IType): Seq[PaletteEntry] = {
    t.getMethods filter { m ⇒
      Flags.isStatic(m.getFlags()) &&
        m.getAnnotations.exists(a ⇒
          a.getElementName == classOf[Box].getName ||
            a.getElementName == "Box")
    } map (PaletteEntry.static(Name(t.getFullyQualifiedName), _))
  }
  def load(t: IType): Seq[PaletteEntry] = {
    val me = try {
      val classname = Name(t.getFullyQualifiedName)
      if (Expressions.find(classname).isDefined) {
        Seq(PaletteEntry.expression(classname))
      } else {
        Seq(PaletteEntry.box(classname))
      }
    } catch { case ex: Exception ⇒ Seq() }
    me ++ loadStatic(t)
  }
}
