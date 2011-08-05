package org.zaluum.nide.zge;

/* 
 * JFreeChart : a free chart library for the Java(tm) platform
 * 
 *
 * (C) Copyright 2000-2007, by Object Refinery Limited and Contributors.
 *
 * Project Info:  http://www.jfree.org/jfreechart/index.html
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 * [Java is a trademark or registered trademark of Sun Microsystems, Inc.
 * in the United States and other countries.]
 *
 * -------------
 * SWTUtils.java
 * -------------
 * (C) Copyright 2006, 2007, by Henry Proudhon and Contributors.
 *
 * Original Author:  Henry Proudhon (henry.proudhon AT ensmp.fr);
 * Contributor(s):   Rainer Blessing;
 *                   David Gilbert (david.gilbert@object-refinery.com);
 *                   Christoph Beck.
 *
 * Changes
 * -------
 * 01-Aug-2006 : New class (HP);
 * 16-Jan-2007 : Use FontData.getHeight() instead of direct field access (RB);
 * 31-Jan-2007 : Moved the dummy JPanel from SWTGraphics2D.java,
 *               added a new convert method for mouse events (HP);
 * 12-Jul-2007 : Improved the mouse event conversion with buttons
 *               and modifiers handling, patch sent by Christoph Beck (HP);
 * 27-Aug-2007 : Modified toAwtMouseEvent signature (HP);
 * 27-Nov-2007 : Moved convertToSWT() method from SWTGraphics2D and added
 *               convertAWTImageToSWT() (DG);
 * 01-Jul-2008 : Simplify AWT/SWT font style conversions (HP);
 *
 */

import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;

import javax.swing.JPanel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;

/**
 * Utility class gathering some useful and general method. Mainly convert forth
 * and back graphical stuff between awt and swt.
 */
public class SWTUtils {

	/** A dummy JPanel used to provide font metrics. */
	protected static final JPanel DUMMY_PANEL = new JPanel();

	/**
	 * Creates an AWT <code>MouseEvent</code> from a swt event. This method
	 * helps passing SWT mouse event to awt components.
	 * 
	 * @param event
	 *            The swt event.
	 * @return A AWT mouse event based on the given SWT event.
	 */
	public static MouseEvent toAwtMouseEvent(
			org.eclipse.swt.events.MouseEvent event) {
		int button = MouseEvent.NOBUTTON;
		switch (event.button) {
		case 1:
			button = MouseEvent.BUTTON1;
			break;
		case 2:
			button = MouseEvent.BUTTON2;
			break;
		case 3:
			button = MouseEvent.BUTTON3;
			break;
		}
		int modifiers = 0;
		if ((event.stateMask & SWT.CTRL) != 0) {
			modifiers |= InputEvent.CTRL_DOWN_MASK;
		}
		if ((event.stateMask & SWT.SHIFT) != 0) {
			modifiers |= InputEvent.SHIFT_DOWN_MASK;
		}
		if ((event.stateMask & SWT.ALT) != 0) {
			modifiers |= InputEvent.ALT_DOWN_MASK;
		}
		MouseEvent awtMouseEvent = new MouseEvent(DUMMY_PANEL,
				event.hashCode(), event.time, modifiers, event.x, event.y, 1,
				false, button);
		return awtMouseEvent;
	}

	/**
	 * Converts an AWT image to SWT.
	 * 
	 * @param image
	 *            the image (<code>null</code> not permitted).
	 * 
	 * @return Image data.
	 */
	public static ImageData convertAWTImageToSWT(Image image) {
		if (image == null) {
			throw new IllegalArgumentException("Null 'image' argument.");
		}
		int w = image.getWidth(null);
		int h = image.getHeight(null);
		if (w == -1 || h == -1) {
			return null;
		}
		BufferedImage bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
		Graphics g = bi.getGraphics();
		g.drawImage(image, 0, 0, null);
		g.dispose();
		return convertToSWT(bi);
	}

	/**
	 * Converts a buffered image to SWT <code>ImageData</code>.
	 * 
	 * @param bufferedImage
	 *            the buffered image (<code>null</code> not permitted).
	 * 
	 * @return The image data.
	 */
	public static ImageData convertToSWT(BufferedImage bufferedImage) {
		if (bufferedImage.getColorModel() instanceof DirectColorModel) {
			DirectColorModel colorModel = (DirectColorModel) bufferedImage
					.getColorModel();
			PaletteData palette = new PaletteData(colorModel.getRedMask(),
					colorModel.getGreenMask(), colorModel.getBlueMask());
			ImageData data = new ImageData(bufferedImage.getWidth(),
					bufferedImage.getHeight(), colorModel.getPixelSize(),
					palette);
			WritableRaster raster = bufferedImage.getRaster();
			int[] pixelArray = new int[3];
			for (int y = 0; y < data.height; y++) {
				for (int x = 0; x < data.width; x++) {
					raster.getPixel(x, y, pixelArray);
					int pixel = palette.getPixel(new RGB(pixelArray[0],
							pixelArray[1], pixelArray[2]));
					data.setPixel(x, y, pixel);
				}
			}
			return data;
		} else if (bufferedImage.getColorModel() instanceof IndexColorModel) {
			IndexColorModel colorModel = (IndexColorModel) bufferedImage
					.getColorModel();
			int size = colorModel.getMapSize();
			byte[] reds = new byte[size];
			byte[] greens = new byte[size];
			byte[] blues = new byte[size];
			colorModel.getReds(reds);
			colorModel.getGreens(greens);
			colorModel.getBlues(blues);
			RGB[] rgbs = new RGB[size];
			for (int i = 0; i < rgbs.length; i++) {
				rgbs[i] = new RGB(reds[i] & 0xFF, greens[i] & 0xFF,
						blues[i] & 0xFF);
			}
			PaletteData palette = new PaletteData(rgbs);
			ImageData data = new ImageData(bufferedImage.getWidth(),
					bufferedImage.getHeight(), colorModel.getPixelSize(),
					palette);
			data.transparentPixel = colorModel.getTransparentPixel();
			WritableRaster raster = bufferedImage.getRaster();
			int[] pixelArray = new int[1];
			for (int y = 0; y < data.height; y++) {
				for (int x = 0; x < data.width; x++) {
					raster.getPixel(x, y, pixelArray);
					data.setPixel(x, y, pixelArray[0]);
				}
			}
			return data;
		}
		return null;
	}

}