package org.zaluum.widget;

import java.awt.Image;
import java.net.URL;

import javax.imageio.ImageIO;

public class ImageReader {
	public static Image readImageResource(String resource) {
		try {
			URL url = Thread.currentThread().getContextClassLoader()
					.getResource(resource);
			return readImageURL(url);
		} catch (Exception e) {
			return null;
		}
	}

	public static Image readImageURL(URL url) {
		try {
			return ImageIO.read(url);
		} catch (Exception e) {
			return null;
		}
	}

}
