package org.zaluum.ide

import org.eclipse.gef.palette.ConnectionCreationToolEntry;
import org.eclipse.gef.palette.CreationToolEntry;
import org.eclipse.gef.palette.PaletteContainer;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.gef.palette.PaletteToolbar;
import org.eclipse.gef.palette.PanningSelectionToolEntry;
import org.eclipse.gef.palette.ToolEntry;
import org.eclipse.gef.requests.CreationFactory;
import org.eclipse.gef.tools.AbstractTool;

object Palette {
  def apply() = {
    val palette = new PaletteRoot()
    palette.add({   
      val toolbar = new PaletteToolbar("Tools")
      val tool = new PanningSelectionToolEntry()
      toolbar.add(tool)
      palette.setDefaultEntry(tool)
      toolbar
    });
    palette
  }
}