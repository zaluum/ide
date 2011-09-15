package org.zaluum.examples.controlMixer;

import java.awt.Graphics;
import java.awt.Image;

import javax.swing.JToggleButton;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
@Box
public class ToggleButton extends JToggleButton {
	private static final long serialVersionUID = 1L;
	private Image selectedImage;
	private Image unselectedImage; 
	@Override @Apply
	public boolean isSelected() {
		return super.isSelected();
	}	
	@Override
	protected void paintComponent(Graphics g) {
		g.setColor(getBackground());
		if (isSelected())
			if (getSelectedImage()!=null)
				g.drawImage(getSelectedImage(), 0,0,null);
			else {
				g.fill3DRect(0,0,getBounds().width, getBounds().height, true);
			}
		else
			if (getUnselectedImage()!=null)
				g.drawImage(getUnselectedImage(), 0,0,null);
			else 
				g.fill3DRect(0,0,getBounds().width, getBounds().height, false);


	}
	public Image getSelectedImage() {
		return selectedImage;
	}
	public void setSelectedImage(Image selectedImage) {
		System.out.println("set" + selectedImage);
		this.selectedImage = selectedImage;
	}
	public Image getUnselectedImage() {
		return unselectedImage;
	}
	public void setUnselectedImage(Image unselectedImage) {
		this.unselectedImage = unselectedImage;
	}
}
