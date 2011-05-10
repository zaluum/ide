package org.zaluum.example;

import org.zaluum.annotation.*;

/**
 * Hola!
 * @author frede
 *
 */
@Box
public class Switch {
	 @In public boolean ab;
	 @In public double a;
	 @In public double b;
	 @Out public double out;
	 public void apply() {
		 if (ab) out=a; else out=b;
	 }
}
