package org.zaluum.nide
import org.zaluum.nide.compiler.{ Point ⇒ MPoint }
import org.zaluum.nide.compiler.Dimension
import org.eclipse.draw2d.geometry.{ Dimension ⇒ EDimension }
import org.eclipse.draw2d.geometry.{ Point ⇒ EPoint }
import org.eclipse.draw2d.IFigure
package object zge {
  def point(p: MPoint): EPoint = new EPoint(p.x, p.y)
  def dimension(d: Dimension): EDimension = new EDimension(d.w, d.h)
  implicit def rpoint(p: EPoint): MPoint = MPoint(p.x, p.y)
  implicit def rdimension(d: EDimension): Dimension = Dimension(d.width, d.height)
  implicit def richFigure(f: IFigure) = new RichFigure(f)

}