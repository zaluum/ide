package org.zaluum.nide.compiler;

import graystone.zaluum.TestBox;

public class GenerationTemplate {
  public TestBox a = new TestBox();
  public TestBox b = new TestBox();
  public TestBox c = new TestBox();
  
  public void apply() {
    a.apply();
    c.a = a.c;
   /* b.apply();
    c.b = b.c;
    c.apply();*/
  }
   
}
