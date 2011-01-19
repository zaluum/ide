package org.zaluum.example;

import org.zaluum.nide.java.Box;
import org.zaluum.nide.java.In;

@Box(image="")
public class TestJava {
  @In(x=0,y=0)  
  public double d = 0.0;
  public void apply() {
	  System.out.println("hola TestJava");
  }
}
