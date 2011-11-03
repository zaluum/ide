package org.zaluum.nide.palette
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.utils.SWTScala._
import org.zaluum.nide.utils.FixedGridLayout
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.Layout
import org.eclipse.swt.graphics.Point
import org.zaluum.nide.eclipse.ImageFactory
import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.nide.zge.TreeViewer
import org.eclipse.draw2d.ColorConstants

trait ZaluumBasePalettePopup extends BasePalettePopup[Pkg] {
  def imgFactory: ImageFactory
  def minWidth = 48 * 4 + 24
  def createPopup(k: Pkg): ZaluumPalettePopup = {
    new ZaluumPalettePopup(k, this)
  }
  def drop(p: PaletteEntry)
  def fillButtons(c: Composite, pkg: Pkg) {
    c.setLayout(new FixedGridLayout(4, 48, 48, 2))
    for (p ← pkg.child) {
      val img = imgFactory.folderIcon(p.name)
      val b = newImageButton(c, img, p.name) {
        openChild(p)
      }
      addOnCustomHover(b, 150) {
        openhover(p);
      }
    }
    for (entry ← pkg.entries) {
      val img = imgFactory.iconForPalette(entry)._1
      val b = newImageButton(c, img, entry.name) {
        drop(entry)
      }
    }
  }
}
class ZaluumFirstPalettePopup(shell: Shell, viewer: TreeViewer) extends FirstPalettePopup[Pkg](shell) with ZaluumBasePalettePopup {
  def palette = viewer.zproject.palette
  lazy val imgFactory = new ImageFactory(viewer.zproject.imageFactory, getShell)
  def drop(p: PaletteEntry) {
    viewer.canvas.setFocus()
    viewer.tool.drop(p)
  }
  def name = "Palette"
  def fillBar(parent: Composite) {
    palette.root match {
      case Some(pkg) ⇒
        fillButtons(parent, pkg)
      //fillBarCommon(parent, pkg)
      case None ⇒
    }
  }
}
class ZaluumPalettePopup(pkg: Pkg, parent: ZaluumBasePalettePopup) extends PalettePopup[Pkg](pkg, parent) with ZaluumBasePalettePopup {
  lazy val imgFactory = new ImageFactory(parent.imgFactory, getShell)
  def fillBar(parent: Composite) {
    fillButtons(parent, pkg)
  }
  def name = pkg.name
  def drop(p: PaletteEntry) = parent.drop(p)
}